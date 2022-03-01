module Wizard.Common.AppState exposing
    ( AppState
    , acceptCookies
    , getDashboardWidgets
    , init
    , isFullscreen
    , setCurrentTime
    , setTimeZone
    )

import Browser.Navigation as Navigation exposing (Key)
import Dict
import Json.Decode as D exposing (Error(..))
import Random exposing (Seed)
import Shared.Auth.Session as Session exposing (Session)
import Shared.Common.Navigator exposing (Navigator)
import Shared.Data.BootstrapConfig exposing (BootstrapConfig)
import Shared.Data.BootstrapConfig.DashboardConfig.DashboardWidget exposing (DashboardWidget(..))
import Shared.Provisioning as Provisioning exposing (Provisioning)
import Time
import Wizard.Common.Flags as Flags
import Wizard.Common.Provisioning.DefaultIconSet as DefaultIconSet
import Wizard.Common.Provisioning.DefaultLocale as DefaultLocale
import Wizard.KMEditor.Editor.KMEditorRoute
import Wizard.KMEditor.Routes
import Wizard.Ports as Ports
import Wizard.Projects.Detail.ProjectDetailRoute
import Wizard.Projects.Routes
import Wizard.Routes as Routes


type alias AppState =
    { route : Routes.Route
    , seed : Seed
    , session : Session
    , invalidSession : Bool
    , key : Key
    , apiUrl : String
    , clientUrl : String
    , config : BootstrapConfig
    , provisioning : Provisioning
    , valid : Bool
    , currentTime : Time.Posix
    , timeZone : Time.Zone
    , navigator : Navigator
    , gaEnabled : Bool
    , cookieConsent : Bool
    }


init : D.Value -> Navigation.Key -> ( AppState, Cmd msg )
init flagsValue key =
    let
        flagsResult =
            D.decodeValue Flags.decoder flagsValue

        flagsCmd =
            case flagsResult of
                Ok _ ->
                    Cmd.none

                Err err ->
                    Ports.consoleError (D.errorToString err)

        invalidSession =
            case flagsResult of
                Err (Field "session" _) ->
                    True

                _ ->
                    False

        flags =
            Result.withDefault Flags.default flagsResult

        defaultProvisioning =
            { locale = DefaultLocale.locale
            , iconSet = DefaultIconSet.iconSet
            }

        provisioning =
            Provisioning.foldl
                [ defaultProvisioning
                , flags.localProvisioning
                , flags.provisioning
                ]
    in
    ( { route = Routes.NotFoundRoute
      , seed = Random.initialSeed flags.seed
      , session = Maybe.withDefault Session.init flags.session
      , invalidSession = invalidSession
      , key = key
      , apiUrl = flags.apiUrl
      , clientUrl = flags.clientUrl
      , config = flags.config
      , provisioning = provisioning
      , valid = flags.success
      , currentTime = Time.millisToPosix 0
      , timeZone = Time.utc
      , navigator = flags.navigator
      , gaEnabled = flags.gaEnabled
      , cookieConsent = flags.cookieConsent
      }
    , flagsCmd
    )


setCurrentTime : AppState -> Time.Posix -> AppState
setCurrentTime appState time =
    { appState | currentTime = time }


setTimeZone : AppState -> Time.Zone -> AppState
setTimeZone appState timeZone =
    { appState | timeZone = timeZone }


acceptCookies : AppState -> AppState
acceptCookies appState =
    { appState | cookieConsent = True }


getDashboardWidgets : AppState -> List DashboardWidget
getDashboardWidgets appState =
    let
        role =
            appState.session.user
                |> Maybe.map .role
                |> Maybe.withDefault ""
    in
    appState.config.dashboard.widgets
        |> Maybe.andThen (Dict.get role)
        |> Maybe.withDefault [ WelcomeDashboardWidget ]


isFullscreen : AppState -> Bool
isFullscreen appState =
    let
        allowedFullscreenRoute =
            case appState.route of
                Routes.KMEditorRoute (Wizard.KMEditor.Routes.EditorRoute _ (Wizard.KMEditor.Editor.KMEditorRoute.Edit _)) ->
                    True

                Routes.ProjectsRoute (Wizard.Projects.Routes.DetailRoute _ Wizard.Projects.Detail.ProjectDetailRoute.Questionnaire) ->
                    True

                _ ->
                    False
    in
    allowedFullscreenRoute && appState.session.fullscreen
