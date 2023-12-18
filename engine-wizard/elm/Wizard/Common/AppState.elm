module Wizard.Common.AppState exposing
    ( AppState
    , acceptCookies
    , getClientUrlRoot
    , getUserRole
    , init
    , isFullscreen
    , sessionExpired
    , sessionExpiresSoon
    , sessionRemainingTime
    , setCurrentTime
    , setTimeZone
    )

import Browser.Navigation as Navigation exposing (Key)
import Gettext
import Json.Decode as D exposing (Error(..))
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Shared.Auth.Session as Session exposing (Session)
import Shared.Common.Navigator exposing (Navigator)
import Shared.Data.BootstrapConfig exposing (BootstrapConfig)
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Shared.Provisioning as Provisioning exposing (Provisioning)
import Shared.Utils.Theme as Theme exposing (Theme)
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
    , locale : Gettext.Locale
    , selectedLocale : Maybe String
    , sessionExpiresSoonModalHidden : Bool
    , theme : Theme
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

        theme =
            Theme.Theme
                (LookAndFeelConfig.getPrimaryColor flags.config.lookAndFeel)
                (LookAndFeelConfig.getIllustrationsColor flags.config.lookAndFeel)
    in
    ( { route = Routes.NotFoundRoute
      , seed = Random.initialSeed flags.seed
      , session = Maybe.withDefault (Session.init flags.apiUrl) flags.session
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
      , locale = flags.locale
      , selectedLocale = flags.selectedLocale
      , sessionExpiresSoonModalHidden = False
      , theme = theme
      }
    , flagsCmd
    )


getUserRole : AppState -> String
getUserRole =
    Maybe.unwrap "" .role << .user << .config


getClientUrlRoot : AppState -> String
getClientUrlRoot appState =
    String.replace "/wizard" "" appState.clientUrl


setCurrentTime : AppState -> Time.Posix -> AppState
setCurrentTime appState time =
    { appState | currentTime = time }


setTimeZone : AppState -> Time.Zone -> AppState
setTimeZone appState timeZone =
    { appState | timeZone = timeZone }


acceptCookies : AppState -> AppState
acceptCookies appState =
    { appState | cookieConsent = True }


isFullscreen : AppState -> Bool
isFullscreen appState =
    let
        allowedFullscreenRoute =
            case appState.route of
                Routes.KMEditorRoute (Wizard.KMEditor.Routes.EditorRoute _ (Wizard.KMEditor.Editor.KMEditorRoute.Edit _)) ->
                    True

                Routes.ProjectsRoute (Wizard.Projects.Routes.DetailRoute _ (Wizard.Projects.Detail.ProjectDetailRoute.Questionnaire _)) ->
                    True

                _ ->
                    False
    in
    allowedFullscreenRoute && appState.session.fullscreen


sessionExpiresSoon : AppState -> Bool
sessionExpiresSoon appState =
    Session.expiresSoon appState.currentTime appState.session


sessionExpired : AppState -> Bool
sessionExpired appState =
    Session.expired appState.currentTime appState.session


sessionRemainingTime : AppState -> String
sessionRemainingTime appState =
    let
        expiration =
            Time.posixToMillis appState.session.token.expiresAt

        currentTime =
            Time.posixToMillis appState.currentTime

        timeLeft =
            max 0 (expiration - currentTime)

        timeLeftMin =
            timeLeft
                // (60 * 1000)
                |> String.fromInt

        timeLeftSec =
            modBy 60 (timeLeft // 1000)
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    timeLeftMin ++ ":" ++ timeLeftSec
