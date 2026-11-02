module Dsw exposing
    ( Model
    , Msg
    , main
    )

{-| The application: the wizard, on the design system.

The admin center's shape. Every feature is a router in
`Dsw.Pages.<Feature>` with the same seven functions (§3.1a), and this module is
the only place that knows all of them — it initializes the page for a route,
routes messages and subscriptions to it, draws it in the right frame and asks it
whether leaving is safe. Nothing below this module imports its `Msg`: pages
reach the application through `Common.Data.UpdateConfig`.

What it owns itself: the session (token, logout, expiry), the frame's state and
the dialogs hung off it (about, report issue, what's new, the session warnings),
the appearance, the brand color, and tours.

**Focus follows navigation.** A single-page application replaces the page
without moving focus, so a keyboard user is left on the link they pressed and a
screen reader says nothing. After a route change focus moves to the new page's
heading (`Ui.AppShell.focusOnRouteChangeWhenReady`, which waits for a page
still loading to show it, and whose retries are dropped once the user has
navigated again); a URL with a fragment focuses what
the fragment names instead, which is what makes the skip links work at all
(`Ui.AppShell.focusFragment`). A jump within the page already open — a skip
link, a metric's name on a project's metrics tab, or Back from one — only moves
focus: it neither loads the page again nor asks the unsaved-changes guard,
because nothing is left behind (`isInPageJump`). A change within the same listing — a filter, a
page of results — keeps focus where it is, because the page did not change. The
very first route is not focused: a page that has just loaded starts at the top
of the document, as every other page on the web does. The exception is a page
that creates something (`Routes.isCreateRoute`): it has one job, which starts
with typing, so it opens on its first field — on the first route too
(`Common.Components.Form.focusFirstField`).

-}

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation exposing (Key)
import Common.Api.BuildInfo as BuildInfoApi
import Common.Api.Models.BootstrapConfig as BootstrapConfig
import Common.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Common.Api.Models.BuildInfo as BuildInfo
import Common.Api.Models.Settings.SupportSettings as SupportSettings
import Common.Api.Models.Token exposing (Token)
import Common.Api.Tokens as TokensApi
import Common.Api.Tours as ToursApi
import Common.Api.Users as UsersApi
import Common.Components.AboutModal as AboutModal
import Common.Components.AppFrame as AppFrame
import Common.Components.Form as CommonForm
import Common.Components.NewsModal as NewsModal
import Common.Components.Page as Page
import Common.Components.ReportIssueModal as ReportIssueModal
import Common.Components.SessionModal as SessionModal
import Common.Components.Tour as Tour
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.NotFound as NotFound exposing (NotFound)
import Common.Data.Session as Session exposing (Session)
import Common.Data.TourId as TourId
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.CommandPalette as CommandPaletteComponent
import Common.Pages.KnowledgeModels.Routes as KnowledgeModelsRoutes
import Common.Pages.Layout as Layout
import Common.Pages.Projects.Routes as ProjectsRoutes
import Common.Pages.Public.Routes as PublicRoutes
import Common.Ports.Console as Console
import Common.Ports.Session as SessionPorts
import Common.Ports.SideNav as SideNavPorts
import Common.Ports.Window as Window
import Common.Routes as Routes
import Common.Utils.GuideLinks as GuideLinks
import Common.Utils.TimeUtils as TimeUtils
import Dsw.Data.ClientDefaults as ClientDefaults
import Dsw.Data.GuideLinks as DswGuideLinks
import Dsw.Data.RequestParams as RequestParams
import Dsw.Layouts.AppLayout as AppLayout
import Dsw.Pages as Router
import Html
import Json.Decode as D
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Result.Extra as Result
import Task
import Time
import Ui.AppShell as AppShell
import Ui.CommandPalette as CommandPalette
import Ui.Theme as Theme
import Url exposing (Url)
import Version


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        , update = updateWithTitle
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { appState : AppState
    , route : Routes.Route
    , frame : AppFrame.Model
    , aboutModal : AboutModal.Model
    , reportIssueOpen : Bool

    -- Where the keyboard goes when it wants to be somewhere else. Open is the
    -- application's, not the palette's: what opens it is a shortcut from
    -- anywhere, including from inside a page that knows nothing about it.
    , commandPalette : CommandPalette.Model
    , commandPaletteOpen : Bool
    , newsModal : NewsModal.Model

    -- Set by a page whose one entity the server does not have: the application
    -- shows its Not Found page in place of that page, with the page's own
    -- sentence. Cleared on every route change, because the next route is a
    -- fresh question.
    , notFound : Maybe NotFound
    , notAllowed : Bool

    -- False until the first route has been handled, so a fresh page load does
    -- not pull focus down to the heading.
    , navigated : Bool

    -- Counts URL changes, so a retry to focus the heading of a page that has
    -- since been left is dropped rather than pulling focus back.
    , navigationId : Int

    -- The fragment of the current URL, so that coming back from an in-page
    -- jump (the browser's Back after a skip link or a link to a section) is
    -- told apart from arriving at the page again.
    , urlFragment : Maybe String
    , pages :
        { shared : Router.Model
        }
    }


init : D.Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( initialAppState, appStateCmd ) =
            AppState.init flags key

        -- This client's server has a registry, so its configuration has to say
        -- so: one without the section is not one to start on.
        registryResult =
            D.decodeValue (D.at [ "config", "registry" ] D.value) flags

        registryCmd =
            case registryResult of
                Ok _ ->
                    Cmd.none

                Err err ->
                    Console.consoleError (D.errorToString err)

        -- This client's addresses are the routes' own paths (the default);
        -- what it adds to its requests is its own to say.
        appState =
            { initialAppState
                | requestParams = RequestParams.requestParams
                , clientDefaults = ClientDefaults.clientDefaults
                , guideLinks = GuideLinks.merge initialAppState.guideLinks DswGuideLinks.default
                , config = BootstrapConfig.withDefaultAppTitle "DS Wizard" initialAppState.config
                , valid = initialAppState.valid && Result.isOk registryResult
            }

        ( newsModalModel, newsModalCmd ) =
            if Feature.newsModal appState then
                NewsModal.init appState.newsUrl BuildInfo.client.version (AppState.userPermissions appState)

            else
                ( NewsModal.initialModel, Cmd.none )

        model =
            { appState = appState
            , route = Routes.NotFoundRoute
            , frame =
                AppFrame.init
                    { collapsed = appState.session.sidebarCollapsed
                    , width = appState.sideNavWidth
                    }
            , aboutModal = AboutModal.initialModel
            , reportIssueOpen = False
            , commandPalette = CommandPalette.init
            , commandPaletteOpen = False
            , newsModal = newsModalModel
            , notFound = Nothing
            , notAllowed = False
            , navigated = False
            , navigationId = 0
            , urlFragment = Nothing
            , pages =
                { shared = Router.initialModel appState
                }
            }

        cmd =
            if appState.invalidSession then
                SessionPorts.clearSessionAndReload ()

            else
                let
                    originalRoute =
                        Routes.parse appState url
                in
                Cmd.batch
                    [ decideInitialRoute model url (Routes.routeIfAllowed appState originalRoute) originalRoute
                    , setThemeCmd appState
                    , TimeUtils.getTime OnTime
                    , TimeUtils.getTimeZone OnTimeZone
                    ]
    in
    ( model
    , Cmd.batch
        [ cmd
        , appStateCmd
        , registryCmd
        , Cmd.map NewsModalMsg newsModalCmd
        ]
    )


{-| The tenant's brand color, when it has one, becomes the seed the design
system derives every brand role from, in both modes. The wizard's own default
color is not a choice anyone made, so it leaves the stylesheet's brand in place
(#F15A24, `scss/_brand.scss`).
-}
setThemeCmd : AppState -> Cmd msg
setThemeCmd appState =
    let
        lookAndFeel =
            appState.config.lookAndFeel
    in
    if LookAndFeelConfig.isCustomTheme lookAndFeel then
        Theme.setTheme (Theme.withSeed (LookAndFeelConfig.getPrimaryColor lookAndFeel) Theme.default)

    else
        Cmd.none


{-| The wizard's rules. A signed-in user asking for a public-only page goes to
the dashboard (the OpenID callback excepted, since that is how they are signing
in). Project and knowledge model pages that may be public are shown to anyone
and decide for themselves. Everything else needs a session, and without one the
user is sent to log in and brought back afterwards.
-}
decideInitialRoute : Model -> Url -> Routes.Route -> Routes.Route -> Cmd Msg
decideInitialRoute model location route originalRoute =
    let
        dispatchUrlChange =
            Task.perform OnUrlChange (Task.succeed location)
    in
    case route of
        Routes.PublicRoute subroute ->
            case ( userLoggedIn model, subroute ) of
                ( _, PublicRoutes.OpenIdCallback _ _ _ _ _ ) ->
                    dispatchUrlChange

                ( True, _ ) ->
                    Routes.navigate model.appState Routes.DashboardRoute

                _ ->
                    dispatchUrlChange

        Routes.ProjectsRoute (ProjectsRoutes.DetailRoute _ _) ->
            dispatchUrlChange

        Routes.ProjectsRoute (ProjectsRoutes.FileDownloadRoute _ _) ->
            dispatchUrlChange

        Routes.KnowledgeModelsRoute (KnowledgeModelsRoutes.DetailRoute _ _) ->
            dispatchUrlChange

        Routes.KnowledgeModelsRoute (KnowledgeModelsRoutes.PreviewRoute _ _) ->
            dispatchUrlChange

        Routes.KnowledgeModelsRoute (KnowledgeModelsRoutes.ResourcePageRoute _ _) ->
            dispatchUrlChange

        _ ->
            if userLoggedIn model then
                dispatchUrlChange

            else
                Routes.cmdNavigateToLogin model.appState (Just (Routes.toUrl model.appState originalRoute))


userLoggedIn : Model -> Bool
userLoggedIn model =
    Session.exists model.appState.session


focusRetryConfig : Int -> { onRetry : AppShell.FocusRetry -> Msg, onDone : Msg }
focusRetryConfig navigationId =
    { onRetry = FocusHeadingRetry navigationId, onDone = NoOp }


{-| A link to a place on the page already open — a skip link, a metric's name
on the metrics tab — or the browser's Back from one: the route stays the same
and there is a fragment to go to, or one to come back from. Never the first
route, which has a page to load whatever its fragment says.

The route rather than the whole URL is compared because the route is what a page
is loaded for.

-}
isInPageJump : Model -> Url -> Bool
isInPageJump model url =
    model.navigated
        && (Maybe.isJust url.fragment || Maybe.isJust model.urlFragment)
        && (Routes.routeIfAllowed model.appState (Routes.parse model.appState url) == model.route)


{-| A link into the API — a download, say — leaves the client even though it is
on the same origin: the client serves every other path, but not those.
-}
isApiUrl : AppState -> Url -> Bool
isApiUrl appState url =
    String.startsWith appState.apiUrl (Url.toString url)


setRoute : Routes.Route -> Model -> Model
setRoute route model =
    { model | route = route }


setSeed : Seed -> Model -> Model
setSeed seed model =
    let
        appState =
            model.appState
    in
    { model | appState = { appState | seed = seed } }


setSession : Session -> Model -> Model
setSession session model =
    let
        appState =
            model.appState
    in
    { model | appState = { appState | session = session } }


addTour : String -> Model -> Model
addTour tourId model =
    let
        appState =
            model.appState
    in
    { model | appState = { appState | config = BootstrapConfig.addTour tourId appState.config } }



-- UPDATE


type Msg
    = NoOp
    | OnUrlChange Url
    | SetNotFound NotFound
    | SetNotAllowed
    | OnUrlRequest UrlRequest
    | FocusHeadingRetry Int AppShell.FocusRetry
    | HistoryBackCallback String
    | OnTime Time.Posix
    | OnTimeZone Time.Zone
    | AuthGotToken Token (Maybe String)
    | AuthLogout
    | AuthLogoutToLogin (Maybe String)
    | AuthLogoutDone
    | ReloadConfig
    | HideSessionExpiresSoonModal
    | FrameMsg AppFrame.Msg
    | SetMode Theme.Mode
    | OpenCommandPalette
    | OpenCommandPaletteFromMenu
    | CommandPaletteMsg CommandPalette.Msg
    | OpenAboutModal
    | AboutModalMsg AboutModal.Msg
    | SetReportIssueOpen Bool
    | OpenNewsModal
    | NewsModalMsg NewsModal.Msg
    | SetLastSeenNewsId String
    | SetLastSeenNewsIdCompleted
    | TourDone String
    | TourPutCompleted
    | PagesSharedMsg Router.Msg


updateConfig : (pageMsg -> Msg) -> UpdateConfig pageMsg Msg
updateConfig wrapMsg =
    { wrapMsg = wrapMsg
    , logoutMsg = AuthLogout
    , logoutToLoginMsg = AuthLogoutToLogin
    , gotTokenMsg = AuthGotToken
    , openNewsModalMsg = OpenNewsModal
    , reloadConfigMsg = ReloadConfig
    , setNotFoundMsg = SetNotFound
    , setNotAllowedMsg = SetNotAllowed
    , entityWorkspaceMsg = always NoOp
    }


{-| The title is set as soon as it changes — the route, or the entity of a
detail page arriving — not only when the view is next drawn: a tab opened in
the background is not drawn until it is shown, and would say the title of the
page it started from until then.
-}
updateWithTitle : Msg -> Model -> ( Model, Cmd Msg )
updateWithTitle msg model =
    let
        ( newModel, cmd ) =
            update msg model
    in
    if newModel.navigated && (windowTitle newModel /= windowTitle model || not model.navigated) then
        ( newModel, Cmd.batch [ cmd, Window.setTitle (windowTitle newModel) ] )

    else
        ( newModel, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        -- The session countdown and every relative time read the clock, so it
        -- is refreshed after anything happens, as the wizard does.
        wrapGetTime ( m, cmd ) =
            case msg of
                OnTime _ ->
                    ( m, cmd )

                _ ->
                    ( m, Cmd.batch [ TimeUtils.getTime OnTime, cmd ] )
    in
    wrapGetTime <|
        case msg of
            NoOp ->
                ( model, Cmd.none )

            OnUrlChange url ->
                if isInPageJump model url then
                    -- The same page, somewhere else on it: focus what the
                    -- fragment names, or on the way back from it nothing at
                    -- all. Loading the page again would throw away what is on
                    -- it, and with it the element the jump was to.
                    ( { model | navigationId = model.navigationId + 1, urlFragment = url.fragment }
                    , if Maybe.isJust url.fragment then
                        Cmd.batch [ CommonForm.cancelFocusFirstField, AppShell.focusFragment NoOp url.fragment ]

                      else
                        Cmd.none
                    )

                else
                    let
                        nextRoute =
                            Routes.routeIfAllowed model.appState (Routes.parse model.appState url)

                        originalRoute =
                            model.route

                        navigationId =
                            model.navigationId + 1

                        modelWithRoute =
                            setRoute nextRoute { model | notFound = Nothing, notAllowed = False, navigated = True, navigationId = navigationId, urlFragment = url.fragment }

                        sameListing =
                            Routes.isSameListingRoute originalRoute nextRoute

                        focusCmd =
                            if Maybe.isJust url.fragment then
                                Cmd.batch [ CommonForm.cancelFocusFirstField, AppShell.focusFragment NoOp url.fragment ]

                            else if Routes.isCreateRoute nextRoute && originalRoute /= nextRoute then
                                -- A create page opens on its first field, the very
                                -- first route included.
                                CommonForm.focusFirstField

                            else if model.navigated && not sameListing && originalRoute /= nextRoute then
                                Cmd.batch
                                    [ CommonForm.cancelFocusFirstField
                                    , AppShell.focusOnRouteChangeWhenReady (focusRetryConfig navigationId)
                                    ]

                            else
                                Cmd.none
                    in
                    if sameListing then
                        ( modelWithRoute, focusCmd )

                    else
                        let
                            ( newModel, pageCmd ) =
                                initPage modelWithRoute
                        in
                        ( newModel, Cmd.batch [ pageCmd, focusCmd ] )

            SetNotFound message ->
                ( { model | notFound = Just message }, Cmd.none )

            SetNotAllowed ->
                ( { model | notAllowed = True }, Cmd.none )

            FocusHeadingRetry navigationId retry ->
                if navigationId == model.navigationId then
                    ( model, AppShell.retryFocusOnRouteChange (focusRetryConfig navigationId) retry )

                else
                    ( model, Cmd.none )

            OnUrlRequest urlRequest ->
                case urlRequest of
                    Browser.Internal url ->
                        if isInPageJump model url then
                            -- Nothing is left behind, so there is nothing to guard.
                            ( model, Navigation.pushUrl model.appState.key (Url.toString url) )

                        else if not (isApiUrl model.appState url) then
                            case isGuarded (Just (Routes.parse model.appState url)) model of
                                Just guardMsg ->
                                    ( model, Window.alert guardMsg )

                                Nothing ->
                                    ( model, Navigation.pushUrl model.appState.key (Url.toString url) )

                        else
                            ( model, Navigation.load (Url.toString url) )

                    Browser.External url ->
                        if url == "" then
                            ( model, Cmd.none )

                        else
                            ( model, Navigation.load url )

            HistoryBackCallback url ->
                if String.isEmpty url then
                    ( model, Navigation.back model.appState.key 1 )

                else
                    ( model, Navigation.pushUrl model.appState.key url )

            OnTime time ->
                ( { model | appState = AppState.setCurrentTime model.appState time }, Cmd.none )

            OnTimeZone timeZone ->
                ( { model | appState = AppState.setTimeZone model.appState timeZone }, Cmd.none )

            AuthGotToken token mbOriginalUrl ->
                let
                    newModel =
                        setSession (Session.setToken token model.appState.session) model

                    redirectUrl =
                        case mbOriginalUrl of
                            Just originalUrl ->
                                if String.startsWith "/open-id" originalUrl then
                                    Routes.toUrl model.appState Routes.DashboardRoute

                                else
                                    originalUrl

                            Nothing ->
                                Routes.toUrl model.appState Routes.DashboardRoute

                    ( newsModalModel, newsModalCmd ) =
                        if Feature.newsModal newModel.appState then
                            NewsModal.init newModel.appState.newsUrl BuildInfo.client.version (AppState.userPermissions newModel.appState)

                        else
                            ( NewsModal.initialModel, Cmd.none )
                in
                ( { newModel | newsModal = newsModalModel }
                , Cmd.batch
                    [ SessionPorts.storeSession (Session.encode newModel.appState.session)
                    , Navigation.load redirectUrl
                    , Cmd.map NewsModalMsg newsModalCmd
                    ]
                )

            AuthLogout ->
                logoutWith (Routes.navigate model.appState Routes.publicLogoutSuccessful) model

            AuthLogoutToLogin mbOriginalUrl ->
                logoutWith (Routes.cmdNavigateToLogin model.appState mbOriginalUrl) model

            AuthLogoutDone ->
                ( model, Cmd.none )

            ReloadConfig ->
                -- The bootstrap configuration is read before the application
                -- starts, so a changed one means starting again.
                ( model, Window.refresh () )

            HideSessionExpiresSoonModal ->
                let
                    appState =
                        model.appState
                in
                ( { model | appState = { appState | sessionExpiresSoonModalHidden = True } }, Cmd.none )

            FrameMsg frameMsg ->
                updateFrame frameMsg model

            SetMode mode ->
                let
                    appState =
                        model.appState
                in
                ( { model | appState = { appState | uiMode = mode } }, Theme.setMode mode )

            OpenCommandPalette ->
                -- Emptied each time: the palette is opened to go somewhere,
                -- and what was typed to go somewhere else is in the way.
                ( { model | commandPaletteOpen = True, commandPalette = CommandPalette.init }, Cmd.none )

            -- From the account menu: closed first, so that focus goes back to its
            -- button when the palette closes, as it does for the other dialogs
            -- opened from there.
            OpenCommandPaletteFromMenu ->
                let
                    ( closedModel, closeCmd ) =
                        updateFrame AppFrame.closeUserMenu model

                    ( newModel, openCmd ) =
                        update OpenCommandPalette closedModel
                in
                ( newModel, Cmd.batch [ closeCmd, openCmd ] )

            CommandPaletteMsg paletteMsg ->
                let
                    ( palette, paletteCmd, outcome ) =
                        CommandPalette.update
                            (CommandPaletteComponent.items (always True) model.appState (AppLayout.destinations model.appState) model.commandPalette)
                            paletteMsg
                            model.commandPalette
                in
                case outcome of
                    Just (CommandPalette.Navigate url) ->
                        ( { model | commandPalette = palette, commandPaletteOpen = False }
                        , Navigation.pushUrl model.appState.key url
                        )

                    Just CommandPalette.Close ->
                        ( { model | commandPalette = palette, commandPaletteOpen = False }, Cmd.none )

                    Nothing ->
                        ( { model | commandPalette = palette }, Cmd.map CommandPaletteMsg paletteCmd )

            OpenAboutModal ->
                let
                    ( closedModel, closeCmd ) =
                        updateFrame AppFrame.closeUserMenu model

                    ( newModel, aboutCmd ) =
                        updateAboutModal AboutModal.open closedModel
                in
                ( newModel, Cmd.batch [ closeCmd, aboutCmd ] )

            AboutModalMsg aboutModalMsg ->
                updateAboutModal aboutModalMsg model

            SetReportIssueOpen open ->
                if open then
                    let
                        ( closedModel, closeCmd ) =
                            updateFrame AppFrame.closeUserMenu model
                    in
                    ( { closedModel | reportIssueOpen = True }, closeCmd )

                else
                    ( { model | reportIssueOpen = False }, Cmd.none )

            OpenNewsModal ->
                let
                    ( closedModel, closeCmd ) =
                        updateFrame AppFrame.closeUserMenu model

                    ( newModel, newsCmd ) =
                        updateNewsModal NewsModal.open closedModel
                in
                ( newModel, Cmd.batch [ closeCmd, newsCmd ] )

            NewsModalMsg newsModalMsg ->
                updateNewsModal newsModalMsg model

            SetLastSeenNewsId lastSeenId ->
                ( model
                , UsersApi.putLastSeenNewsId model.appState lastSeenId (always SetLastSeenNewsIdCompleted)
                )

            SetLastSeenNewsIdCompleted ->
                ( model, Cmd.none )

            TourDone tourId ->
                ( addTour tourId model
                , ToursApi.putTour model.appState tourId (always TourPutCompleted)
                )

            TourPutCompleted ->
                ( model, Cmd.none )

            PagesSharedMsg pageMsg ->
                let
                    ( seed, pageModel, pageCmd ) =
                        Router.update (updateConfig PagesSharedMsg) model.appState pageMsg model.pages.shared

                    pages =
                        model.pages
                in
                ( setSeed seed { model | pages = { pages | shared = pageModel } }, pageCmd )


{-| Revoke the token and forget the session. Navigating within the client does
not have to wait for the revocation.
-}
logoutWith : Cmd Msg -> Model -> ( Model, Cmd Msg )
logoutWith navigateCmd model =
    ( setSession (Session.init model.appState.apiUrl) model
    , Cmd.batch
        [ SessionPorts.clearSession ()
        , TokensApi.deleteCurrentToken model.appState (always AuthLogoutDone)
        , navigateCmd
        ]
    )


{-| The frame keeps its own state; the collapsed flag is also written back to
the session, which is where it has always been stored and what anything else
saving the session will write.
-}
updateFrame : AppFrame.Msg -> Model -> ( Model, Cmd Msg )
updateFrame frameMsg model =
    let
        session =
            model.appState.session

        ( frame, frameCmd ) =
            AppFrame.update
                { storeCollapsed = \collapsed -> SessionPorts.storeSession (Session.encode (Session.setSidebarCollapsed collapsed session))
                , storeWidth = \width -> SideNavPorts.uiStoreSideNav { collapsed = session.sidebarCollapsed, width = width }
                , toMsg = FrameMsg
                }
                frameMsg
                model.frame
    in
    ( setSession (Session.setSidebarCollapsed (AppFrame.isCollapsed frame) session) { model | frame = frame }
    , frameCmd
    )


updateAboutModal : AboutModal.Msg -> Model -> ( Model, Cmd Msg )
updateAboutModal aboutModalMsg model =
    let
        ( aboutModal, aboutModalCmd ) =
            AboutModal.update model.appState
                { getBuildInfo = BuildInfoApi.getBuildInfo model.appState }
                aboutModalMsg
                model.aboutModal
    in
    ( { model | aboutModal = aboutModal }, Cmd.map AboutModalMsg aboutModalCmd )


updateNewsModal : NewsModal.Msg -> Model -> ( Model, Cmd Msg )
updateNewsModal newsModalMsg model =
    let
        dashboardTourActive =
            model.appState.config.features.toursEnabled
                && (model.route == Routes.DashboardRoute)
                && List.notMember TourId.dashboard model.appState.config.tours

        ( newsModal, newsModalCmd ) =
            NewsModal.update
                { lastSeenId = Maybe.andThen .lastSeenNewsId model.appState.config.user
                , setLastSeenMsg = SetLastSeenNewsId
                , wrapMsg = NewsModalMsg
                , dashboardTourActive = dashboardTourActive
                }
                newsModalMsg
                model.newsModal
    in
    ( { model | newsModal = newsModal }, newsModalCmd )


{-| Hand a new route to its feature router. A route within the same listing
never gets here: the listing already has its data and only its query changed.
-}
initPage : Model -> ( Model, Cmd Msg )
initPage model =
    let
        pages =
            model.pages

        ( pageModel, pageCmd ) =
            Router.init model.appState model.route model.pages.shared
    in
    ( { model | pages = { pages | shared = pageModel } }, Cmd.map PagesSharedMsg pageCmd )


{-| Whether the page being left has unsaved changes. `mbNextRoute` is where the
user is going, for a page that cares — the project detail holds them only while
its changes are saving and they are leaving the project.
-}
isGuarded : Maybe Routes.Route -> Model -> Maybe String
isGuarded mbNextRoute model =
    Router.isGuarded model.appState mbNextRoute model.route model.pages.shared



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        pageSubscriptions =
            Sub.map PagesSharedMsg (Router.subscriptions model.appState model.route model.pages.shared)

        -- Ticks every second only while the countdown is on screen.
        sessionSubscriptions =
            if Session.exists model.appState.session && AppState.sessionExpiresSoon model.appState then
                Time.every 1000 OnTime

            else
                Sub.none
    in
    Sub.batch
        [ pageSubscriptions
        , sessionSubscriptions
        , Sub.map FrameMsg (AppFrame.subscriptions model.frame)
        , Sub.map NewsModalMsg (NewsModal.subscriptions model.newsModal)
        , Window.historyBackCallback HistoryBackCallback
        , Tour.onDone TourDone
        ]



-- VIEW


view : Model -> Document Msg
view model =
    if model.appState.valid then
        withWindowTitle model (viewApp model)

    else
        AppLayout.misconfigured model.appState


{-| Every frame's title is the window's (`windowTitle`), whichever layout drew
the page.
-}
withWindowTitle : Model -> Document Msg -> Document Msg
withWindowTitle model document =
    { document | title = windowTitle model }


{-| The window's title: the page's, named after what it is about where it has
a name (a detail page's entity, once it has arrived). The application's name
alone until the address has been routed: the first view is drawn before that,
while the route is still the placeholder Not Found, and a tab opened in the
background draws nothing more until it is shown.
-}
windowTitle : Model -> String
windowTitle model =
    if model.navigated then
        Layout.documentTitle model.appState
            (Maybe.withDefault (Layout.pageName model.appState model.route)
                (Router.entityName model.appState model.pages.shared model.route)
            )

    else
        LookAndFeelConfig.getAppTitle model.appState.config.lookAndFeel


viewApp : Model -> Document Msg
viewApp model =
    let
        appState =
            model.appState

        loggedIn =
            Session.exists appState.session

        logoutToLoginMsg =
            AuthLogoutToLogin (Just (Routes.toUrl appState model.route))

        support =
            appState.config.support

        -- Always in the document, open or not: a dialog that is added at the
        -- moment it opens has nothing for the browser to return focus from.
        overlays =
            [ ReportIssueModal.view appState
                { open = model.reportIssueOpen
                , onClose = SetReportIssueOpen False
                , supportSiteName = SupportSettings.getSupportSiteName appState.clientDefaults support
                , supportSiteUrl = SupportSettings.getSupportSiteUrl appState.clientDefaults support
                , supportSiteIcon = SupportSettings.getSupportSiteIcon appState.clientDefaults support
                , supportEmail = SupportSettings.getSupportEmail appState.clientDefaults support
                }
            , AboutModal.view appState
                { appTitle = Nothing
                , apiUrl = appState.apiUrl
                , swaggerUrl = Just (appState.apiUrl ++ "/swagger-ui/")
                , plugins = List.map (\plugin -> { name = plugin.name, version = Version.toString plugin.version }) appState.pluginMetadata
                , toMsg = AboutModalMsg
                }
                model.aboutModal
            , SessionModal.expiresSoon appState
                { open =
                    loggedIn
                        && AppState.sessionExpiresSoon appState
                        && not (AppState.sessionExpired appState)
                        && not appState.sessionExpiresSoonModalHidden
                , onLogin = logoutToLoginMsg
                , onClose = HideSessionExpiresSoonModal
                }
            , SessionModal.expired appState
                { open = loggedIn && AppState.sessionExpired appState
                , onLogin = logoutToLoginMsg
                }
            , Html.map NewsModalMsg (NewsModal.view appState model.newsModal)
            , CommandPaletteComponent.shortcut OpenCommandPalette
            , CommandPaletteComponent.view appState
                { open = model.commandPaletteOpen
                , navigation = AppLayout.destinations appState
                , reachable = always True
                , wrapMsg = CommandPaletteMsg
                , onClose = CommandPaletteMsg CommandPalette.dismissed
                }
                model.commandPalette
            ]

        appConfig =
            { frame = model.frame
            , frameMsg = FrameMsg
            , setMode = SetMode
            , logoutMsg = AuthLogout
            , openAboutMsg = OpenAboutModal
            , openCommandPaletteMsg = OpenCommandPaletteFromMenu
            , openNewsMsg = OpenNewsModal
            , openReportIssueMsg = SetReportIssueOpen True
            , footerNotice =
                if loggedIn && AppState.sessionExpiresSoon appState && not (AppState.sessionExpired appState) then
                    Just
                        (SessionModal.expiryNotice appState
                            { remaining = AppState.sessionRemainingTime appState
                            , onLogin = logoutToLoginMsg
                            }
                        )

                else
                    Nothing
            , overlays = overlays
            , route = model.route
            }
    in
    case model.notFound of
        -- A page asked for one thing and the server does not have it. The
        -- address stays as it is, so a reload asks again and a corrected link
        -- still works; only what is drawn changes.
        Just value ->
            Page.notFoundEntity appState
                { message = value.message
                , back = NotFound.back appState value
                }
                |> AppLayout.mixedApp appState appConfig

        Nothing ->
            -- A page asked for one thing and the server refuses it to this
            -- reader; the address stays, as for one that is not there.
            if model.notAllowed then
                Page.notAllowed appState { homeUrl = notAllowedHomeUrl appState }
                    |> AppLayout.mixedApp appState appConfig

            else
                viewRoute model appState appConfig


{-| Where the Not Allowed page leads: the dashboard for someone signed in, and
nowhere for anyone else, whom the dashboard would turn away the same way.
-}
notAllowedHomeUrl : AppState -> Maybe String
notAllowedHomeUrl appState =
    if Session.exists appState.session then
        Just (Routes.toUrl appState Routes.appHome)

    else
        Nothing


{-| The page the address names. Its own function so that the application can
draw something else in its place — a thing that is not there — without every
branch below knowing about it.
-}
viewRoute : Model -> AppState -> AppLayout.AppConfig Msg -> Document Msg
viewRoute model appState appConfig =
    case model.route of
        Routes.NotAllowedRoute ->
            Page.notAllowed appState { homeUrl = notAllowedHomeUrl appState }
                |> AppLayout.mixedApp appState appConfig

        route ->
            case Router.view appState route model.pages.shared of
                Just content ->
                    let
                        html =
                            Html.map PagesSharedMsg content
                    in
                    case Router.frame route of
                        Router.AppFrame ->
                            AppLayout.app appState appConfig html

                        Router.MixedFrame ->
                            AppLayout.mixedApp appState appConfig html

                        Router.PublicFrame ->
                            AppLayout.public appState { route = model.route, overlays = [] } html

                Nothing ->
                    Page.notFound appState { homeUrl = Routes.toUrl appState Routes.appHome }
                        |> AppLayout.mixedApp appState appConfig
