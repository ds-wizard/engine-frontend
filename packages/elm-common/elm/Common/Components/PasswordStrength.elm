module Common.Components.PasswordStrength exposing
    ( Model
    , Msg
    , PasswordField
    , Updated
    , init
    , isChecking
    , toCmd
    , toUiStrength
    , update
    , updateForm
    , validation
    )

{-| The strength of a new password, as the server sees it.

Where a client turns on `Extensions.passwordStrengthCheck`, the server holds a
new password to its own measure, and the meter has to show that measure rather
than one worked out in the browser, which disagrees with it often enough to
matter. The password is sent once typing pauses (500 ms); the meter is busy
until the answer for the password in the field arrives, and an answer for one
that is no longer there is dropped.

The verdict is part of the form's validation, as the local one is where the
check is off: a password the server has not called strong does not pass, so
the form is not sent. Its error is `FormError.PasswordTooWeak`, for which the
field shows no message of its own: the meter, which already says how strong
the password is, says that it is not accepted (`toUiStrength`), and the field
is invalid. One the server could not be asked
about passes, and the server decides when the form arrives. While the answer is
still to come, nothing is said under the field — the meter says it is checking
— and a submit waits for the answer instead: it goes ahead once the answer is
in, and fails as any other submit of an invalid form if the answer is no.
Changing the password while it waits takes the submit back.

A page keeps a `Model` beside its form. It sends the form only when the output
is there and `isChecking` is false; every other form message goes to
`updateForm`, and every message of this one to `update`. Both validate the form
again with what is known now, and `toCmd` turns what they return into the
page's commands, the waiting submit among them. The page passes the model to
`Common.Components.Form.Field.passwordWithStrength`. Where the client has not
turned the check on, the form is validated as it would be without this, and the
field keeps its local meter.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Context exposing (Context)
import Common.Api.Models.PasswordStrength exposing (PasswordStrength(..))
import Common.Api.PasswordStrength as PasswordStrengthApi
import Common.Data.Context exposing (WithLocale)
import Common.Data.Extensions exposing (Extensions)
import Common.Utils.Form.FormError as FormError exposing (FormError)
import Debouncer exposing (Debouncer)
import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Gettext exposing (gettext)
import Task.Extra as Task
import Ui.Field as UiField


type Model
    = Model
        { enabled : Bool
        , password : String
        , verdict : ActionResult PasswordStrength
        , debouncer : Debouncer Msg
        , submitWhenChecked : Bool
        }


init : { a | extensions : Extensions } -> Model
init appState =
    Model
        { enabled = appState.extensions.passwordStrengthCheck
        , password = ""
        , verdict = Unset
        , debouncer = Debouncer.toDebouncer (Debouncer.debounce 500)
        , submitWhenChecked = False
        }


type Msg
    = Apply String
    | Completed String (Result ApiError PasswordStrength)
    | DebouncerMsg (Debouncer.Msg Msg)


{-| Where the password is in a form, and the form's own validation.
-}
type alias PasswordField o =
    { path : String
    , validation : Validation FormError o
    }


{-| Whether the answer for the password in the field is still to come: a
submit then waits for it.
-}
isChecking : Model -> Bool
isChecking (Model state) =
    state.enabled && ActionResult.isLoading state.verdict


{-| The form and the model after a message, the command of this component, and
whether a submit that waited for the answer is to go ahead now.
-}
type alias Updated o =
    { form : Form FormError o
    , passwordStrength : Model
    , cmd : Cmd Msg
    , submit : Bool
    }


{-| A message of the form: applied, the password in it asked about once typing
pauses if it is not the one already asked about, and the form validated again
with what is known of it now. A submit while the answer is still to come is
remembered, to go ahead once it is in.
-}
updateForm : Context a -> PasswordField o -> Form.Msg -> Form FormError o -> Model -> Updated o
updateForm appState field formMsg form model =
    let
        updatedForm =
            Form.update (validation field model) formMsg form

        ( (Model checkedState) as checkedModel, cmd ) =
            check appState (Maybe.withDefault "" (Form.getFieldAsString field.path updatedForm).value) model

        waitingModel =
            case formMsg of
                Form.Submit ->
                    if isChecking checkedModel then
                        Model { checkedState | submitWhenChecked = True }

                    else
                        checkedModel

                _ ->
                    checkedModel
    in
    { form = revalidate field waitingModel updatedForm
    , passwordStrength = waitingModel
    , cmd = cmd
    , submit = False
    }


{-| A message of this component: the form validated again, since the answer
may have arrived, and a submit that waited for it let go ahead.
-}
update : Context a -> PasswordField o -> Msg -> Form FormError o -> Model -> Updated o
update appState field msg form model =
    let
        ( (Model updatedState) as updatedModel, cmd ) =
            updateModel appState msg model

        submit =
            updatedState.submitWhenChecked && not (isChecking updatedModel)

        finalModel =
            if submit then
                Model { updatedState | submitWhenChecked = False }

            else
                updatedModel
    in
    { form = revalidate field finalModel form
    , passwordStrength = finalModel
    , cmd = cmd
    , submit = submit
    }


{-| The page's commands: this component's, wrapped, and the submit message
again when a submit that waited is to go ahead.
-}
toCmd : (Msg -> msg) -> msg -> Updated o -> Cmd msg
toCmd wrapMsg submitMsg updated =
    if updated.submit then
        Cmd.batch [ Cmd.map wrapMsg updated.cmd, Task.dispatch submitMsg ]

    else
        Cmd.map wrapMsg updated.cmd


revalidate : PasswordField o -> Model -> Form FormError o -> Form FormError o
revalidate field model =
    Form.update (validation field model) Form.Validate


{-| The form's own validation, and the server's verdict on the password. The
form's own error on the field, an empty one, say, comes first.
-}
validation : PasswordField o -> Model -> Validation FormError o
validation field (Model state) =
    let
        verdict =
            if not state.enabled || String.isEmpty state.password then
                V.succeed ()

            else
                case state.verdict of
                    Success Strong ->
                        V.succeed ()

                    Success _ ->
                        V.field field.path (V.fail (V.customError FormError.PasswordTooWeak))

                    _ ->
                        V.succeed ()
    in
    V.map2 always field.validation verdict


check : Context a -> String -> Model -> ( Model, Cmd Msg )
check appState password ((Model state) as model) =
    if not state.enabled || password == state.password then
        ( model, Cmd.none )

    else if String.isEmpty password then
        ( Model { state | password = password, verdict = Unset, submitWhenChecked = False }, Cmd.none )

    else
        updateModel appState
            (DebouncerMsg (Debouncer.provideInput (Apply password)))
            (Model { state | password = password, verdict = Loading, submitWhenChecked = False })


updateModel : Context a -> Msg -> Model -> ( Model, Cmd Msg )
updateModel appState msg ((Model state) as model) =
    case msg of
        Apply password ->
            if password == state.password && not (String.isEmpty password) then
                ( model, PasswordStrengthApi.postPasswordStrength appState password (Completed password) )

            else
                ( model, Cmd.none )

        Completed password result ->
            if password == state.password then
                case result of
                    Ok strength ->
                        ( Model { state | verdict = Success strength }, Cmd.none )

                    Err _ ->
                        ( Model { state | verdict = Error "" }, Cmd.none )

            else
                ( model, Cmd.none )

        DebouncerMsg debouncerMsg ->
            Debouncer.update (updateModel appState)
                { mapMsg = DebouncerMsg
                , getDebouncer = \(Model s) -> s.debouncer
                , setDebouncer = \debouncer (Model s) -> Model { s | debouncer = debouncer }
                }
                debouncerMsg
                model


{-| What the meter shows. `Nothing` where the client has not turned the check
on: the field then works the strength out itself. A bar for each of the five
verdicts, `Risky` lighting the first; the tone is set rather than left to the
fraction, since only `Strong` is accepted: everything below it is the weak
tone, four bars of `Safe` included: the form does not take them.

The words say so from the first answer: they are what describes the field
when it turns invalid on being left (`Ui.Field.withStrengthAsError`), and have
to say that the password is not accepted, which "almost there" did not — and
words that changed only then would read as a second verdict on the same
password.

-}
toUiStrength : WithLocale a -> Model -> Maybe UiField.Strength
toUiStrength appState (Model state) =
    if not state.enabled then
        Nothing

    else
        Just <|
            case state.verdict of
                Success strength ->
                    let
                        ( level, tone, label ) =
                            case strength of
                                Risky ->
                                    ( 1, UiField.WeakTone, gettext "Password too weak" appState.locale )

                                Weak ->
                                    ( 2, UiField.WeakTone, gettext "Password too weak" appState.locale )

                                Moderate ->
                                    ( 3, UiField.WeakTone, gettext "Not strong enough yet" appState.locale )

                                Safe ->
                                    ( 4, UiField.WeakTone, gettext "Not strong enough yet" appState.locale )

                                Strong ->
                                    ( 5, UiField.StrongTone, gettext "Strong password" appState.locale )
                    in
                    UiField.strengthScoredAs tone { level = level, max = bars, label = label }

                Error _ ->
                    UiField.strengthUnavailable { max = bars, label = gettext "Error checking password strength" appState.locale }

                _ ->
                    UiField.strengthChecking { max = bars }


{-| One bar for each verdict, in every state, so the meter keeps its length
while it waits.
-}
bars : Int
bars =
    5
