module Registry.Pages.Login exposing
    ( LoginForm
    , Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, form, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onSubmit)
import Registry.Common.AppState exposing (AppState)
import Registry.Common.Credentials exposing (Credentials)
import Registry.Common.Requests as Requests
import Registry.Common.View.ActionButton as ActionButton
import Registry.Common.View.FormGroup as FormGroup
import Registry.Common.View.FormResult as FormResult
import Registry.Routing as Routing
import Shared.Error.ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)


init : Model
init =
    { form = initLoginForm
    , loggingIn = Unset
    }



-- MODEL


type alias Model =
    { form : Form FormError LoginForm
    , loggingIn : ActionResult ()
    }


type alias LoginForm =
    { organizationId : String
    , token : String
    }


loginFormValidation : Validation e LoginForm
loginFormValidation =
    Validate.map2 LoginForm
        (Validate.field "organizationId" Validate.string)
        (Validate.field "token" Validate.string)


initLoginForm : Form e LoginForm
initLoginForm =
    Form.initial [] loginFormValidation



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | GetTokenCompleted (Result ApiError String)


update :
    { tagger : Msg -> msg
    , loginCmd : Credentials -> Cmd msg
    }
    -> Msg
    -> AppState
    -> Model
    -> ( Model, Cmd msg )
update { tagger, loginCmd } msg appState model =
    case msg of
        FormMsg formMsg ->
            handleFormMsg tagger formMsg appState model

        GetTokenCompleted result ->
            case ( result, Form.getOutput model.form ) of
                ( Ok _, Just loginForm ) ->
                    ( model, loginCmd loginForm )

                _ ->
                    ( { model | loggingIn = Error <| gettext "Login failed." appState.locale }
                    , Cmd.none
                    )


handleFormMsg : (Msg -> msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd msg )
handleFormMsg tagger formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just loginForm ) ->
            ( { model | loggingIn = Loading }
            , Requests.getToken loginForm appState GetTokenCompleted
                |> Cmd.map tagger
            )

        _ ->
            ( { model | form = Form.update loginFormValidation formMsg model.form }
            , Cmd.none
            )



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    div [ class "card card-form bg-light" ]
        [ div [ class "card-header" ] [ text (gettext "Log In" appState.locale) ]
        , div [ class "card-body" ]
            [ form [ onSubmit <| FormMsg Form.Submit ]
                [ FormResult.errorOnlyView model.loggingIn
                , Html.map FormMsg <| FormGroup.input appState model.form "organizationId" <| gettext "Organization ID" appState.locale
                , Html.map FormMsg <| FormGroup.password appState model.form "token" <| gettext "Token" appState.locale
                , div [ class "d-flex justify-content-between align-items-center" ]
                    [ ActionButton.submit ( gettext "Log In" appState.locale, model.loggingIn )
                    , a [ href <| Routing.toString Routing.ForgottenToken ] [ text (gettext "Forgot your token?" appState.locale) ]
                    ]
                ]
            ]
        ]
