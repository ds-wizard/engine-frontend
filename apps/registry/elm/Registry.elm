module Registry exposing
    ( Model
    , Msg
    , Page
    , main
    )

{-| The registry's web client, on the design system.

What is published can be read by anybody; an account makes API keys
for wizards, and an administrator manages the accounts. Publishing is not done
here: an administrator publishes from a wizard.

What this module owns: the session, the person signed in, the frame, and which
page is open. A signed-in session is checked first (`GET /users/current`): the
first page waits for it, since what it may show depends on who it is.

Focus follows navigation as in the other clients: after a route change it
moves to the new page's heading (`Ui.AppShell.focusOnRouteChangeWhenReady`);
the very first page starts at the top of the document.

-}

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation exposing (Key)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.Token as Token
import Common.Components.AppFrame as AppFrame
import Common.Components.Page as Page
import Common.Data.Session as Session
import Common.Ports.Session as SessionPorts
import Common.Ports.SideNav as SideNavPorts
import Common.Utils.TimeUtils as TimeUtils
import Gettext exposing (gettext)
import Html exposing (Html)
import Json.Decode as D
import Maybe.Extra as Maybe
import Registry.Api as Api
import Registry.Api.Models.Item exposing (Kind)
import Registry.Api.Models.User exposing (User)
import Registry.Data.AppState as AppState exposing (AppState)
import Registry.Data.Kind as Kind
import Registry.Layout as Layout
import Registry.Pages.Account as Account
import Registry.Pages.ApiKeys as ApiKeys
import Registry.Pages.Confirmation as Confirmation
import Registry.Pages.ForgottenPassword as ForgottenPassword
import Registry.Pages.Home as Home
import Registry.Pages.ItemDetail as ItemDetail
import Registry.Pages.Items as Items
import Registry.Pages.Login as Login
import Registry.Pages.ResetPassword as ResetPassword
import Registry.Pages.Signup as Signup
import Registry.Pages.Users as Users
import Registry.Routes as Routes exposing (Route)
import Time
import Ui.AppShell as AppShell
import Ui.Theme as Theme
import Url exposing (Url)


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Page
    = HomePage
    | ItemsPage Items.Model
    | ItemDetailPage ItemDetail.Model
    | LoginPage Login.Model
    | SignupPage Signup.Model
    | ConfirmationPage Confirmation.Model
    | ForgottenPasswordPage ForgottenPassword.Model
    | ResetPasswordPage ResetPassword.Model
    | AccountPage Account.Model
    | ApiKeysPage ApiKeys.Model
    | UsersPage Users.Model
    | NotFoundPage
    | NotAllowedPage


type alias Model =
    { appState : AppState
    , route : Route
    , page : Page
    , frame : AppFrame.Model

    -- The address the registry was opened at, while the person signed in is
    -- still being asked for.
    , pendingUrl : Maybe Url
    , navigated : Bool
    , navigationId : Int
    }


init : D.Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        appState =
            AppState.init flags key

        model =
            { appState = appState
            , route = Routes.NotFound
            , page = NotFoundPage
            , frame = AppFrame.init { collapsed = appState.session.sidebarCollapsed, width = appState.sideNavWidth }
            , pendingUrl = Nothing
            , navigated = False
            , navigationId = 0
            }
    in
    if not appState.valid then
        ( model, Cmd.none )

    else
        let
            timeCmd =
                Cmd.batch [ TimeUtils.getTime OnTime, TimeUtils.getTimeZone OnTimeZone ]
        in
        if AppState.isSignedIn appState then
            ( { model | pendingUrl = Just url }
            , Cmd.batch [ Api.getCurrentUser appState GetCurrentUserCompleted, timeCmd ]
            )

        else
            let
                ( newModel, cmd ) =
                    goTo url model
            in
            -- The first page starts at the top of the document; the next ones
            -- move focus to their heading.
            ( { newModel | navigated = True }, Cmd.batch [ cmd, timeCmd ] )



-- UPDATE


type Msg
    = NoOp
    | OnUrlChange Url
    | OnUrlRequest UrlRequest
    | OnTime Time.Posix
    | OnTimeZone Time.Zone
    | FocusHeadingRetry Int AppShell.FocusRetry
    | GetCurrentUserCompleted (Result ApiError User)
    | Logout
    | FrameMsg AppFrame.Msg
    | SetMode Theme.Mode
    | ItemsMsg Items.Msg
    | ItemDetailMsg ItemDetail.Msg
    | LoginMsg Login.Msg
    | SignupMsg Signup.Msg
    | ConfirmationMsg Confirmation.Msg
    | ForgottenPasswordMsg ForgottenPassword.Msg
    | ResetPasswordMsg ResetPassword.Msg
    | AccountMsg Account.Msg
    | ApiKeysMsg ApiKeys.Msg
    | UsersMsg Users.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( NoOp, _ ) ->
            ( model, Cmd.none )

        ( OnUrlChange url, _ ) ->
            let
                navigationId =
                    model.navigationId + 1

                ( newModel, pageCmd ) =
                    goTo url { model | navigationId = navigationId }

                focusCmd =
                    -- After every navigation but the first, a redirect included:
                    -- the registry has no jumps within a page to leave alone.
                    if model.navigated then
                        AppShell.focusOnRouteChangeWhenReady (focusRetryConfig navigationId)

                    else
                        Cmd.none
            in
            ( newModel, Cmd.batch [ pageCmd, focusCmd ] )

        ( OnUrlRequest urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.appState.key (Url.toString url) )

                Browser.External url ->
                    if String.isEmpty url then
                        ( model, Cmd.none )

                    else
                        ( model, Navigation.load url )

        ( OnTime time, _ ) ->
            ( updateAppState (\appState -> { appState | currentTime = time }) model, Cmd.none )

        ( OnTimeZone zone, _ ) ->
            ( updateAppState (\appState -> { appState | timeZone = zone }) model, Cmd.none )

        ( FocusHeadingRetry navigationId retry, _ ) ->
            if navigationId == model.navigationId then
                ( model, AppShell.retryFocusOnRouteChange (focusRetryConfig navigationId) retry )

            else
                ( model, Cmd.none )

        ( GetCurrentUserCompleted result, _ ) ->
            case result of
                Ok user ->
                    let
                        withUser =
                            updateAppState (\appState -> { appState | user = Just user }) model
                    in
                    case model.pendingUrl of
                        Just url ->
                            goTo url { withUser | pendingUrl = Nothing, navigated = True }

                        Nothing ->
                            ( withUser, Cmd.none )

                Err error ->
                    -- A session the server no longer knows is no session.
                    if isUnauthorized error then
                        ( model, SessionPorts.clearSessionAndReload () )

                    else
                        case model.pendingUrl of
                            Just url ->
                                goTo url { model | pendingUrl = Nothing, navigated = True }

                            Nothing ->
                                ( model, Cmd.none )

        ( Logout, _ ) ->
            ( model, Cmd.batch [ SessionPorts.clearSession (), Navigation.load (Routes.toUrl Routes.Home) ] )

        ( FrameMsg frameMsg, _ ) ->
            let
                ( frame, cmd ) =
                    AppFrame.update
                        { storeCollapsed = storeCollapsed model
                        , storeWidth = \width -> SideNavPorts.uiStoreSideNav { collapsed = AppFrame.isCollapsed model.frame, width = width }
                        , toMsg = FrameMsg
                        }
                        frameMsg
                        model.frame
            in
            ( { model | frame = frame }, cmd )

        ( SetMode mode, _ ) ->
            ( updateAppState (\appState -> { appState | uiMode = mode }) model, Theme.setMode mode )

        ( ItemsMsg pageMsg, ItemsPage pageModel ) ->
            ( { model | page = ItemsPage (Items.update model.appState pageMsg pageModel) }, Cmd.none )

        ( ItemDetailMsg pageMsg, ItemDetailPage pageModel ) ->
            let
                ( newPage, cmd ) =
                    ItemDetail.update model.appState pageMsg pageModel
            in
            ( { model | page = ItemDetailPage newPage }, Cmd.map ItemDetailMsg cmd )

        ( LoginMsg pageMsg, LoginPage pageModel ) ->
            let
                ( newPage, cmd, mbToken ) =
                    Login.update model.appState pageMsg pageModel
            in
            case mbToken of
                Just token ->
                    let
                        session =
                            Session.setToken (Token.create token.token token.expiresAt) model.appState.session

                        target =
                            case model.route of
                                Routes.Login (Just originalUrl) ->
                                    originalUrl

                                _ ->
                                    Routes.toUrl Routes.Home
                    in
                    ( { model | page = LoginPage newPage }
                    , Cmd.batch [ SessionPorts.storeSession (Session.encode session), Navigation.load target ]
                    )

                Nothing ->
                    ( { model | page = LoginPage newPage }, Cmd.map LoginMsg cmd )

        ( SignupMsg pageMsg, SignupPage pageModel ) ->
            let
                ( newPage, cmd ) =
                    Signup.update model.appState pageMsg pageModel
            in
            ( { model | page = SignupPage newPage }, Cmd.map SignupMsg cmd )

        ( ConfirmationMsg pageMsg, ConfirmationPage pageModel ) ->
            ( { model | page = ConfirmationPage (Confirmation.update model.appState pageMsg pageModel) }, Cmd.none )

        ( ForgottenPasswordMsg pageMsg, ForgottenPasswordPage pageModel ) ->
            let
                ( newPage, cmd ) =
                    ForgottenPassword.update model.appState pageMsg pageModel
            in
            ( { model | page = ForgottenPasswordPage newPage }, Cmd.map ForgottenPasswordMsg cmd )

        ( ResetPasswordMsg pageMsg, ResetPasswordPage pageModel ) ->
            let
                ( newPage, cmd ) =
                    ResetPassword.update model.appState pageMsg pageModel
            in
            ( { model | page = ResetPasswordPage newPage }, Cmd.map ResetPasswordMsg cmd )

        ( AccountMsg pageMsg, AccountPage pageModel ) ->
            let
                ( newPage, cmd, mbUser ) =
                    Account.update model.appState pageMsg pageModel

                withUser =
                    case mbUser of
                        Just user ->
                            updateAppState (\appState -> { appState | user = Just user }) model

                        Nothing ->
                            model
            in
            ( { withUser | page = AccountPage newPage }, Cmd.map AccountMsg cmd )

        ( ApiKeysMsg pageMsg, ApiKeysPage pageModel ) ->
            let
                ( newPage, cmd ) =
                    ApiKeys.update model.appState pageMsg pageModel
            in
            ( { model | page = ApiKeysPage newPage }, Cmd.map ApiKeysMsg cmd )

        ( UsersMsg pageMsg, UsersPage pageModel ) ->
            let
                ( newPage, cmd ) =
                    Users.update model.appState pageMsg pageModel
            in
            ( { model | page = UsersPage newPage }, Cmd.map UsersMsg cmd )

        _ ->
            ( model, Cmd.none )


isUnauthorized : ApiError -> Bool
isUnauthorized error =
    case error of
        ApiError.BadStatus 401 _ ->
            True

        _ ->
            False


updateAppState : (AppState -> AppState) -> Model -> Model
updateAppState f model =
    { model | appState = f model.appState }


storeCollapsed : Model -> Bool -> Cmd Msg
storeCollapsed model collapsed =
    let
        session =
            model.appState.session
    in
    SessionPorts.storeSession (Session.encode { session | sidebarCollapsed = collapsed })


focusRetryConfig : Int -> { onRetry : AppShell.FocusRetry -> Msg, onDone : Msg }
focusRetryConfig navigationId =
    { onRetry = FocusHeadingRetry navigationId, onDone = NoOp }


{-| The page of an address. Someone signed in is sent home from the pages for
signing in; someone not signed in is sent to log in from a page that needs an
account, and brought back afterwards.
-}
goTo : Url -> Model -> ( Model, Cmd Msg )
goTo url model =
    let
        appState =
            model.appState

        route =
            Routes.parse appState url

        signedIn =
            AppState.isSignedIn appState

        withPage page =
            { model | route = route, page = page }
    in
    if signedIn && Routes.isPublicOnly route then
        ( model, Navigation.replaceUrl appState.key (Routes.toUrl Routes.Home) )

    else if not signedIn && not (Routes.isAllowed appState route) then
        ( model, Navigation.replaceUrl appState.key (Routes.toUrl (Routes.Login (Just (stripOrigin url)))) )

    else if not (Routes.isAllowed appState route) then
        ( withPage NotAllowedPage, Cmd.none )

    else
        case route of
            Routes.Home ->
                ( withPage HomePage, Cmd.none )

            Routes.Items kind ->
                initPage withPage ItemsPage ItemsMsg (Items.init appState kind)

            Routes.ItemDetail kind reference ->
                initPage withPage ItemDetailPage ItemDetailMsg (ItemDetail.init appState kind reference)

            Routes.Login _ ->
                ( withPage (LoginPage Login.init), Cmd.none )

            Routes.Signup ->
                ( withPage (SignupPage Signup.init), Cmd.none )

            Routes.SignupConfirmation uuid hash ->
                initPage withPage ConfirmationPage ConfirmationMsg (Confirmation.init appState { uuid = uuid, hash = hash })

            Routes.ForgottenPassword ->
                ( withPage (ForgottenPasswordPage ForgottenPassword.init), Cmd.none )

            Routes.ResetPassword uuid hash ->
                ( withPage (ResetPasswordPage (ResetPassword.init { uuid = uuid, hash = hash })), Cmd.none )

            Routes.Account ->
                ( withPage (AccountPage (Account.init appState)), Cmd.none )

            Routes.ApiKeys ->
                initPage withPage ApiKeysPage ApiKeysMsg (ApiKeys.init appState)

            Routes.Users ->
                initPage withPage UsersPage UsersMsg (Users.init appState)

            Routes.NotFound ->
                ( withPage NotFoundPage, Cmd.none )


initPage : (Page -> Model) -> (pageModel -> Page) -> (pageMsg -> Msg) -> ( pageModel, Cmd pageMsg ) -> ( Model, Cmd Msg )
initPage withPage toPage toMsg ( pageModel, cmd ) =
    ( withPage (toPage pageModel), Cmd.map toMsg cmd )


{-| The path and query of an address, to come back to after signing in.
-}
stripOrigin : Url -> String
stripOrigin url =
    url.path
        ++ (case url.query of
                Just query ->
                    "?" ++ query

                Nothing ->
                    ""
           )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if AppState.isSignedIn model.appState then
        Sub.map FrameMsg (AppFrame.subscriptions model.frame)

    else
        Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    let
        appState =
            model.appState

        documentTitle pageTitle =
            pageTitle ++ " | " ++ gettext "Registry" appState.locale
    in
    if not appState.valid then
        { title = gettext "Configuration Error" appState.locale
        , body = [ Page.misconfigured appState ]
        }

    else if Maybe.isJust model.pendingUrl then
        { title = gettext "Registry" appState.locale
        , body = [ Page.loading appState ]
        }

    else
        let
            formPage =
                case model.page of
                    LoginPage pageModel ->
                        Just (mapForm LoginMsg (Login.view appState pageModel))

                    SignupPage pageModel ->
                        Just (mapForm SignupMsg (Signup.view appState pageModel))

                    ConfirmationPage pageModel ->
                        Just (mapForm ConfirmationMsg (Confirmation.view appState pageModel))

                    ForgottenPasswordPage pageModel ->
                        Just (mapForm ForgottenPasswordMsg (ForgottenPassword.view appState pageModel))

                    ResetPasswordPage pageModel ->
                        Just (mapForm ResetPasswordMsg (ResetPassword.view appState pageModel))

                    _ ->
                        Nothing
        in
        case formPage of
            Just page ->
                { title = documentTitle page.title
                , body = [ Layout.form appState page ]
                }

            Nothing ->
                let
                    content =
                        case model.page of
                            HomePage ->
                                ( gettext "Registry" appState.locale, Home.view appState )

                            ItemsPage pageModel ->
                                ( kindTitle appState pageModel.kind, Html.map ItemsMsg (Items.view appState pageModel) )

                            ItemDetailPage pageModel ->
                                ( kindTitle appState pageModel.kind, Html.map ItemDetailMsg (ItemDetail.view appState pageModel) )

                            AccountPage pageModel ->
                                ( gettext "Account" appState.locale, Html.map AccountMsg (Account.view appState pageModel) )

                            ApiKeysPage pageModel ->
                                ( gettext "API Keys" appState.locale, Html.map ApiKeysMsg (ApiKeys.view appState pageModel) )

                            UsersPage pageModel ->
                                ( gettext "Users" appState.locale, Html.map UsersMsg (Users.view appState pageModel) )

                            NotAllowedPage ->
                                ( gettext "Not Allowed" appState.locale, Page.notAllowed appState { homeUrl = Just (Routes.toUrl Routes.Home) } )

                            _ ->
                                ( gettext "Not Found" appState.locale, Page.notFound appState { homeUrl = Routes.toUrl Routes.Home } )

                    ( pageTitle, html ) =
                        content
                in
                { title = documentTitle pageTitle
                , body =
                    [ if AppState.isSignedIn appState then
                        Layout.signedIn appState
                            { frame = model.frame
                            , frameMsg = FrameMsg
                            , setMode = SetMode
                            , logoutMsg = Logout
                            , route = model.route
                            }
                            [ html ]

                      else
                        Layout.browse appState [ html ]
                    ]
                }


mapForm : (msg -> Msg) -> { title : String, content : List (Html msg) } -> { title : String, content : List (Html Msg) }
mapForm toMsg page =
    { title = page.title, content = List.map (Html.map toMsg) page.content }


kindTitle : AppState -> Kind -> String
kindTitle appState kind =
    Kind.title appState kind
