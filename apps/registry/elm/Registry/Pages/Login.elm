module Registry.Pages.Login exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

{-| Signing in with an e-mail and a password (the registry has accounts,
not organizations with a token). The token it gets is the application's to keep
(`update` hands it over).
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
import Time
import Ui.Link as Link


type alias Model =
    { email : String
    , password : String
    , submitted : Bool
    , result : ActionResult ()
    }


init : Model
init =
    { email = "", password = "", submitted = False, result = Unset }


type Msg
    = SetEmail String
    | SetPassword String
    | Submit
    | Completed (Result ApiError { token : String, expiresAt : Time.Posix })


errors : AppState -> Model -> { email : Maybe String, password : Maybe String }
errors appState model =
    { email = Fields.emailError appState model.email
    , password = Fields.requiredError appState model.password
    }


{-| The token once the server gave one.
-}
update : AppState -> Msg -> Model -> ( Model, Cmd Msg, Maybe { token : String, expiresAt : Time.Posix } )
update appState msg model =
    case msg of
        SetEmail email ->
            ( { model | email = email }, Cmd.none, Nothing )

        SetPassword password ->
            ( { model | password = password }, Cmd.none, Nothing )

        Submit ->
            if ActionResult.isLoading model.result then
                ( model, Cmd.none, Nothing )

            else
                let
                    found =
                        errors appState model
                in
                if Maybe.isJust found.email || Maybe.isJust found.password then
                    ( { model | submitted = True }, Cmd.none, Nothing )

                else
                    ( { model | submitted = True, result = Loading }
                    , Api.postToken appState { email = String.trim model.email, password = model.password } Completed
                    , Nothing
                    )

        Completed result ->
            case result of
                Ok token ->
                    ( { model | result = Success () }, Cmd.none, Just token )

                Err error ->
                    ( { model | result = ApiError.toActionResult appState (gettext "Login failed." appState.locale) error }
                    , Form.focusErrorSummary
                    , Nothing
                    )


view : AppState -> Model -> { title : String, content : List (Html Msg) }
view appState model =
    let
        found =
            if model.submitted then
                errors appState model

            else
                { email = Nothing, password = Nothing }

        signup =
            if appState.config.publicRegistrationEnabled then
                [ p []
                    [ Link.link { label = gettext "Sign up" appState.locale, url = Routes.toUrl Routes.Signup } |> Link.toHtml ]
                ]

            else
                []
    in
    { title = gettext "Log in" appState.locale
    , content =
        Form.view
            { onSubmit = Submit
            , content =
                [ Form.errorSummary model.result
                , Fields.field { id = "email", label = gettext "Email" appState.locale, kind = Fields.Email, value = model.email, onInput = SetEmail, error = found.email }
                , Fields.field { id = "password", label = gettext "Password" appState.locale, kind = Fields.CurrentPassword, value = model.password, onInput = SetPassword, error = found.password }
                , Form.actions appState { submitLabel = gettext "Log in" appState.locale, result = model.result, onCancel = Nothing }
                ]
            }
            :: p [] [ Link.link { label = gettext "Forgot your password?" appState.locale, url = Routes.toUrl Routes.ForgottenPassword } |> Link.toHtml ]
            :: signup
    }
