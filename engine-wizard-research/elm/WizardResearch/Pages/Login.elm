module WizardResearch.Pages.Login exposing
    ( Model
    , Msg
    , UpdateConfig
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Html.Styled exposing (Html, a, div, h1, p, text)
import Html.Styled.Attributes exposing (href, style)
import Shared.Api.Auth as AuthApi
import Shared.Api.Tokens as TokensApi
import Shared.Api.Users as UsersApi
import Shared.Data.BootstrapConfig
import Shared.Data.BootstrapConfig.AuthenticationConfig
import Shared.Data.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig
import Shared.Data.Token as Token exposing (Token)
import Shared.Data.UserInfo exposing (UserInfo)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Html.Styled exposing (emptyNode, fa)
import WizardResearch.Common.AppState exposing (AppState)
import WizardResearch.Common.Session exposing (Session)
import WizardResearch.Pages.Login.LoginForm as LoginForm exposing (LoginForm)



-- MODEL


type alias Model =
    { loginForm : Form FormError LoginForm
    , rawToken : String
    , token : ActionResult Token
    , userInfo : ActionResult UserInfo
    }


init : ( Model, Cmd Msg )
init =
    ( { loginForm = LoginForm.init
      , rawToken = ""
      , token = Unset
      , userInfo = Unset
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | FetchTokenComplete (Result ApiError Token)
    | GetUserInfoComplete (Result ApiError UserInfo)


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , onAuthenticate : Session -> Cmd msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        FormMsg formMsg ->
            updateWith cfg <|
                handleFormMsg appState formMsg model

        FetchTokenComplete result ->
            updateWith cfg <|
                handleFetchTokenComplete appState result model

        GetUserInfoComplete result ->
            handleGetUserInfoComplete cfg result model


updateWith : UpdateConfig msg -> ( Model, Cmd Msg ) -> ( Model, Cmd msg )
updateWith cfg ( model, cmd ) =
    ( model, Cmd.map cfg.wrapMsg cmd )


handleFormMsg : AppState -> Form.Msg -> Model -> ( Model, Cmd Msg )
handleFormMsg appState formMsg model =
    case ( formMsg, Form.getOutput model.loginForm ) of
        ( Form.Submit, Just loginForm ) ->
            ( { model | token = Loading }
            , TokensApi.fetchToken (LoginForm.encode loginForm) appState FetchTokenComplete
            )

        _ ->
            ( { model | loginForm = Form.update LoginForm.validation formMsg model.loginForm }
            , Cmd.none
            )


handleFetchTokenComplete : AppState -> Result ApiError Token -> Model -> ( Model, Cmd Msg )
handleFetchTokenComplete appState result model =
    case result of
        Ok token ->
            let
                rawToken =
                    Token.value token

                apiConfig =
                    { apiUrl = appState.apiConfig.apiUrl, token = rawToken }
            in
            ( { model | userInfo = Loading, token = Success token, rawToken = rawToken }
            , UsersApi.getUserInfo { appState | apiConfig = apiConfig } GetUserInfoComplete
            )

        Err error ->
            ( { model | token = ApiError.toActionResult "Unable to get token" error }
            , Cmd.none
            )


handleGetUserInfoComplete : UpdateConfig msg -> Result ApiError UserInfo -> Model -> ( Model, Cmd msg )
handleGetUserInfoComplete cfg result model =
    case result of
        Ok userInfo ->
            ( model
            , cfg.onAuthenticate <| Session model.rawToken userInfo
            )

        Err error ->
            ( { model | userInfo = ApiError.toActionResult "Unable to get user info" error }
            , Cmd.none
            )



-- VIEW


view : AppState -> Model -> { title : String, content : Html Msg }
view appState model =
    let
        actionResults =
            ActionResult.combine model.token model.userInfo

        error =
            case actionResults of
                Error e ->
                    p [] [ text e ]

                _ ->
                    emptyNode

        openIDServices =
            List.map serviceView appState.config.authentication.external.services

        serviceView config =
            a
                [ href <| AuthApi.authRedirectUrl config appState
                , style "padding" "1rem"
                , style "margin-right" "1rem"
                , style "display" "inline-block"
                , style "background" (Maybe.withDefault "#333" config.style.background)
                , style "color" (Maybe.withDefault "#fff" config.style.color)
                ]
                [ fa (Maybe.withDefault "fab fa-openid" config.style.icon)
                , text " "
                , text config.name
                ]
    in
    { title = "Login"
    , content =
        div []
            [ h1 [] [ text "Login" ]
            , error
            , LoginForm.view appState model.loginForm |> Html.Styled.map FormMsg
            , div [] openIDServices
            ]
    }
