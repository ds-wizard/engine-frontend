module Registry.Pages.Account exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

{-| One's own account: the name and e-mail, and a new password. Two forms, each
saved on its own, each saying beside itself whether it was.
-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.Form as Form
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Registry.Api as Api
import Registry.Api.Models.User exposing (User)
import Registry.Components.Fields as Fields
import Registry.Data.AppState exposing (AppState)
import Ui.Alert as Alert
import Ui.Layout as Layout
import Ui.Page.Content as Content
import Ui.Text as Text


type alias Model =
    { firstName : String
    , lastName : String
    , email : String
    , profileSubmitted : Bool
    , profileResult : ActionResult ()
    , password : String
    , repeatedPassword : String
    , passwordSubmitted : Bool
    , passwordResult : ActionResult ()
    }


init : AppState -> Model
init appState =
    let
        field get =
            Maybe.unwrap "" get appState.user
    in
    { firstName = field .firstName
    , lastName = field .lastName
    , email = field .email
    , profileSubmitted = False
    , profileResult = Unset
    , password = ""
    , repeatedPassword = ""
    , passwordSubmitted = False
    , passwordResult = Unset
    }


type Msg
    = SetFirstName String
    | SetLastName String
    | SetEmail String
    | SubmitProfile
    | ProfileCompleted (Result ApiError User)
    | SetPassword String
    | SetRepeatedPassword String
    | SubmitPassword
    | PasswordCompleted (Result ApiError ())


profileErrors : AppState -> Model -> List (Maybe String)
profileErrors appState model =
    [ Fields.requiredError appState model.firstName
    , Fields.requiredError appState model.lastName
    , Fields.emailError appState model.email
    ]


passwordErrors : AppState -> Model -> List (Maybe String)
passwordErrors appState model =
    [ Fields.passwordError appState model.password
    , Fields.repeatError appState model.password model.repeatedPassword
    ]


{-| The account as the server has it after a save, for the application to show.
-}
update : AppState -> Msg -> Model -> ( Model, Cmd Msg, Maybe User )
update appState msg model =
    case msg of
        SetFirstName value ->
            ( { model | firstName = value, profileResult = Unset }, Cmd.none, Nothing )

        SetLastName value ->
            ( { model | lastName = value, profileResult = Unset }, Cmd.none, Nothing )

        SetEmail value ->
            ( { model | email = value, profileResult = Unset }, Cmd.none, Nothing )

        SubmitProfile ->
            if ActionResult.isLoading model.profileResult then
                ( model, Cmd.none, Nothing )

            else if List.all ((==) Nothing) (profileErrors appState model) then
                ( { model | profileSubmitted = True, profileResult = Loading }
                , Api.putCurrentUser appState
                    { email = String.trim model.email, firstName = String.trim model.firstName, lastName = String.trim model.lastName }
                    ProfileCompleted
                , Nothing
                )

            else
                ( { model | profileSubmitted = True }, Cmd.none, Nothing )

        ProfileCompleted (Ok user) ->
            ( { model | profileResult = Success () }, Cmd.none, Just user )

        ProfileCompleted (Err error) ->
            ( { model | profileResult = ApiError.toActionResult appState (gettext "Profile could not be saved." appState.locale) error }
            , Cmd.none
            , Nothing
            )

        SetPassword value ->
            ( { model | password = value, passwordResult = Unset }, Cmd.none, Nothing )

        SetRepeatedPassword value ->
            ( { model | repeatedPassword = value, passwordResult = Unset }, Cmd.none, Nothing )

        SubmitPassword ->
            if ActionResult.isLoading model.passwordResult then
                ( model, Cmd.none, Nothing )

            else if List.all ((==) Nothing) (passwordErrors appState model) then
                ( { model | passwordSubmitted = True, passwordResult = Loading }
                , Api.putCurrentPassword appState model.password PasswordCompleted
                , Nothing
                )

            else
                ( { model | passwordSubmitted = True }, Cmd.none, Nothing )

        PasswordCompleted (Ok _) ->
            ( { model | passwordResult = Success (), password = "", repeatedPassword = "", passwordSubmitted = False }, Cmd.none, Nothing )

        PasswordCompleted (Err error) ->
            ( { model | passwordResult = ApiError.toActionResult appState (gettext "Password could not be saved." appState.locale) error }
            , Cmd.none
            , Nothing
            )


view : AppState -> Model -> Html Msg
view appState model =
    let
        errorIf submitted error =
            if submitted then
                error

            else
                Nothing

        -- Two forms on one page: each says its own outcome beside itself, as
        -- an alert, since the error summary is one element per page.
        outcome result message =
            case result of
                Success _ ->
                    Alert.toHtml (Alert.success message)

                Error error ->
                    Alert.toHtml (Alert.danger error)

                _ ->
                    Html.text ""

        profileForm =
            Form.view
                { onSubmit = SubmitProfile
                , content =
                    [ Text.title (gettext "Profile" appState.locale)
                    , outcome model.profileResult (gettext "Profile was successfully updated." appState.locale)
                    , Fields.field { id = "firstName", label = gettext "First name" appState.locale, kind = Fields.Text "given-name", value = model.firstName, onInput = SetFirstName, error = errorIf model.profileSubmitted (Fields.requiredError appState model.firstName) }
                    , Fields.field { id = "lastName", label = gettext "Last name" appState.locale, kind = Fields.Text "family-name", value = model.lastName, onInput = SetLastName, error = errorIf model.profileSubmitted (Fields.requiredError appState model.lastName) }
                    , Fields.field { id = "email", label = gettext "Email" appState.locale, kind = Fields.Email, value = model.email, onInput = SetEmail, error = errorIf model.profileSubmitted (Fields.emailError appState model.email) }
                    , Form.actions appState { submitLabel = gettext "Save" appState.locale, result = model.profileResult, onCancel = Nothing }
                    ]
                }

        passwordForm =
            Form.view
                { onSubmit = SubmitPassword
                , content =
                    [ Text.title (gettext "Password" appState.locale)
                    , outcome model.passwordResult (gettext "Password was successfully changed." appState.locale)
                    , Fields.field { id = "password", label = gettext "New password" appState.locale, kind = Fields.NewPassword, value = model.password, onInput = SetPassword, error = errorIf model.passwordSubmitted (Fields.passwordError appState model.password) }
                    , Fields.field { id = "passwordConfirmation", label = gettext "New password again" appState.locale, kind = Fields.NewPassword, value = model.repeatedPassword, onInput = SetRepeatedPassword, error = errorIf model.passwordSubmitted (Fields.repeatError appState model.password model.repeatedPassword) }
                    , Form.actions appState { submitLabel = gettext "Save" appState.locale, result = model.passwordResult, onCancel = Nothing }
                    ]
                }
    in
    Content.content
        { title = gettext "Account" appState.locale
        , body = [ Layout.stack [ profileForm, passwordForm ] ]
        }
        |> Content.form
        |> Content.toHtml
