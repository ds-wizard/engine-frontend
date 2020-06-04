module WizardResearch.Pages.Login exposing
    ( Model
    , Msg
    , UpdateConfig
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Css exposing (..)
import Css.Global exposing (descendants, typeSelector)
import Css.Transitions exposing (transition)
import Form exposing (Form)
import Html.Styled exposing (Html, a, div, fromUnstyled, h1, h2, p, span, strong, text)
import Html.Styled.Attributes exposing (css, href, style)
import Markdown
import Shared.Api.Auth as AuthApi
import Shared.Api.Tokens as TokensApi
import Shared.Api.Users as UsersApi
import Shared.Data.BootstrapConfig
import Shared.Data.BootstrapConfig.AuthenticationConfig
import Shared.Data.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig exposing (OpenIDServiceConfig)
import Shared.Data.Token as Token exposing (Token)
import Shared.Data.UserInfo exposing (UserInfo)
import Shared.Elemental.Atoms.Button as Button
import Shared.Elemental.Atoms.Flash as Flash
import Shared.Elemental.Foundations.Animation as Animation
import Shared.Elemental.Foundations.Border as Border
import Shared.Elemental.Foundations.Grid as Grid exposing (Grid)
import Shared.Elemental.Foundations.Illustration as Illustration
import Shared.Elemental.Foundations.Shadow as Shadow
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Foundations.Transition as Transition
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Utils exposing (px2rem)
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
        grid =
            Grid.comfortable
    in
    { title = "Login"
    , content =
        grid.container [ Grid.containerFluid ]
            [ grid.row []
                [ grid.col 12 [ Grid.colVerticalCenter, Grid.fullHeight ] [ panel appState model ]
                ]
            ]
    }


panel : AppState -> Model -> Html Msg
panel appState model =
    let
        grid =
            Grid.comfortable

        styles =
            [ Shadow.xl Shadow.colorDarker appState.theme
            , Border.roundedDefault
            , margin2 (px2rem Spacing.md) auto
            , width (pct 100)
            , maxWidth (px2rem 960)
            ]
    in
    div [ css styles, Animation.fadeIn, Animation.fast ]
        [ grid.block []
            [ grid.row []
                [ infoBlock appState grid
                , loginBlock appState grid model
                ]
            ]
        ]


infoBlock : AppState -> Grid Msg -> Html Msg
infoBlock appState grid =
    let
        infoBlockContent =
            case appState.config.lookAndFeel.loginInfo of
                Just loginInfo ->
                    [ fromUnstyled <| Markdown.toHtml [] loginInfo ]

                Nothing ->
                    defaultInfoBlockContent appState

        blockStyle =
            [ Spacing.insetLG
            , important (paddingRight (px2rem (Spacing.lg - (Spacing.gridComfortable / 2))))
            ]

        h1Style =
            [ important Spacing.stackXL
            , backgroundImage (url appState.theme.logo.url)
            , backgroundRepeat noRepeat
            , backgroundSize2 (px2rem appState.theme.logo.width) (px2rem appState.theme.logo.height)
            , minHeight (px2rem appState.theme.logo.height)
            , displayFlex
            , alignItems center
            , paddingLeft (px2rem (appState.theme.logo.width + 10))
            , textDecoration none
            ]
    in
    grid.col 6
        [ css blockStyle ]
        (h1 [ css h1Style ] [ text "DS Wizard" ] :: infoBlockContent)


defaultInfoBlockContent : AppState -> List (Html Msg)
defaultInfoBlockContent appState =
    let
        illustrationWrapperStyle =
            [ important Spacing.stackXL ]

        phraseStyle =
            [ fontSize (px2rem Typography.sizeLG)
            , descendants [ typeSelector "strong" [ display block ] ]
            , textAlign center
            ]
    in
    [ div [ css illustrationWrapperStyle ] [ Illustration.serverStatus appState.theme ]
    , div [ css phraseStyle ] [ text "Next Generation", strong [] [ text "Data Stewardship Planning" ] ]
    ]


loginBlock : AppState -> Grid Msg -> Model -> Html Msg
loginBlock appState grid model =
    let
        actionResults =
            ActionResult.combine model.token model.userInfo

        error =
            case actionResults of
                Error e ->
                    Flash.danger appState.theme e

                _ ->
                    emptyNode

        blockStyle =
            [ Spacing.insetLG
            , important (paddingLeft (px2rem (Spacing.lg - (Spacing.gridComfortable / 2))))
            , alignItems center
            , position relative
            , after
                [ position absolute
                , left (px2rem -(Spacing.gridComfortable / 2))
                , top zero
                , right zero
                , bottom zero
                , backgroundColor appState.theme.colors.primaryTint
                , property "content" "\" \""
                , zIndex (int -1)
                , borderTopRightRadius (px2rem Border.radiusDefault)
                , borderBottomRightRadius (px2rem Border.radiusDefault)
                ]
            ]

        loginWrapperStyle =
            [ textAlign center
            , maxWidth (px2rem 320)
            , margin auto
            ]

        openIDServices =
            List.map (serviceView appState) appState.config.authentication.external.services
    in
    grid.col 6
        [ css blockStyle, Grid.colVerticalCenter ]
        [ div [ css loginWrapperStyle ]
            [ h2 [] [ text "Log in to DS Wizard" ]
            , error
            , LoginForm.view appState model.loginForm actionResults |> Html.Styled.map FormMsg
            , connectWithSeparator appState
            , div [] openIDServices
            ]
        ]


connectWithSeparator : AppState -> Html Msg
connectWithSeparator appState =
    let
        styles =
            [ Typography.copy1lighter appState.theme
            , Spacing.stackMD
            , displayFlex
            , width (pct 100)
            , before
                [ property "content" "\" \""
                , flex2 (num 1) (num 1)
                , borderBottom3 (px 1) solid appState.theme.colors.textLight
                , margin4 auto (px2rem Spacing.md) auto zero
                ]
            , after
                [ property "content" "\" \""
                , flex2 (num 1) (num 1)
                , borderBottom3 (px 1) solid appState.theme.colors.textLight
                , margin4 auto zero auto (px2rem Spacing.md)
                ]
            ]
    in
    div [ css styles ] [ text "or connect with" ]


serviceView : AppState -> OpenIDServiceConfig -> Html Msg
serviceView appState config =
    let
        color =
            hex <| Maybe.withDefault "#fff" config.style.color

        btnBackgroundColor =
            hex <| Maybe.withDefault "#333" config.style.background

        buttonStyles =
            [ Spacing.stackSM
            , width (pct 100)
            , justifyContent center
            , transition
                [ Transition.default Css.Transitions.opacity3
                , Transition.default Css.Transitions.boxShadow3
                , Transition.default Css.Transitions.transform3
                , Transition.default Css.Transitions.backgroundColor3
                ]
            , hover
                [ important (backgroundColor btnBackgroundColor)
                , opacity (num 0.85)
                ]
            ]
    in
    Button.colorful color
        btnBackgroundColor
        appState.theme
        [ css buttonStyles ]
        [ fa (Maybe.withDefault "fab fa-openid" config.style.icon)
        , span [] [ text config.name ]
        ]
