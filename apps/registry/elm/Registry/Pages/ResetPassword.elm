module Registry.Pages.ResetPassword exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

{-| The link the registry e-mails for a forgotten password: a new password,
twice.
-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.Form as Form
import Gettext exposing (gettext)
import Html exposing (Html, p)
import Maybe.Extra as Maybe
import Registry.Api as Api
import Registry.Components.Fields as Fields
import Registry.Data.AppState exposing (AppState)
import Registry.Routes as Routes
import Ui.Alert as Alert
import Ui.Link as Link


type alias Model =
    { uuid : String
    , hash : String
    , password : String
    , repeatedPassword : String
    , submitted : Bool
    , result : ActionResult ()
    }


init : { uuid : String, hash : String } -> Model
init link =
    { uuid = link.uuid, hash = link.hash, password = "", repeatedPassword = "", submitted = False, result = Unset }


type Msg
    = SetPassword String
    | SetRepeatedPassword String
    | Submit
    | Completed (Result ApiError ())


errors : AppState -> Model -> { password : Maybe String, repeatedPassword : Maybe String }
errors appState model =
    { password = Fields.passwordError appState model.password
    , repeatedPassword = Fields.repeatError appState model.password model.repeatedPassword
    }


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        SetPassword value ->
            ( { model | password = value }, Cmd.none )

        SetRepeatedPassword value ->
            ( { model | repeatedPassword = value }, Cmd.none )

        Submit ->
            if ActionResult.isLoading model.result then
                ( model, Cmd.none )

            else
                let
                    found =
                        errors appState model
                in
                if Maybe.isNothing found.password && Maybe.isNothing found.repeatedPassword then
                    ( { model | submitted = True, result = Loading }
                    , Api.putPassword appState { uuid = model.uuid, hash = model.hash, password = model.password } Completed
                    )

                else
                    ( { model | submitted = True }, Cmd.none )

        Completed (Ok _) ->
            ( { model | result = Success () }, Cmd.none )

        Completed (Err error) ->
            ( { model | result = ApiError.toActionResult appState (gettext "Password recovery failed." appState.locale) error }
            , Form.focusErrorSummary
            )


view : AppState -> Model -> { title : String, content : List (Html Msg) }
view appState model =
    { title = gettext "Recover Password" appState.locale
    , content =
        case model.result of
            Success _ ->
                [ Alert.toHtml (Alert.success (gettext "Your password has been changed. You can now log in." appState.locale))
                , p [] [ Link.link { label = gettext "Log in" appState.locale, url = Routes.toUrl (Routes.Login Nothing) } |> Link.toHtml ]
                ]

            _ ->
                let
                    found =
                        if model.submitted then
                            errors appState model

                        else
                            { password = Nothing, repeatedPassword = Nothing }
                in
                [ Form.view
                    { onSubmit = Submit
                    , content =
                        [ Form.errorSummary model.result
                        , Fields.field { id = "password", label = gettext "New password" appState.locale, kind = Fields.NewPassword, value = model.password, onInput = SetPassword, error = found.password }
                        , Fields.field { id = "passwordConfirmation", label = gettext "New password again" appState.locale, kind = Fields.NewPassword, value = model.repeatedPassword, onInput = SetRepeatedPassword, error = found.repeatedPassword }
                        , Form.actions appState { submitLabel = gettext "Save" appState.locale, result = model.result, onCancel = Nothing }
                        ]
                    }
                ]
    }
