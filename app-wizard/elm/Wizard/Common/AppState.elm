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
    , toAIAssistantServerInfo
    , toServerInfo
    )

import Browser.Navigation as Navigation exposing (Key)
import Gettext
import Json.Decode as D exposing (Error(..))
import Random exposing (Seed)
import Shared.Api.Request exposing (ServerInfo)
import Shared.Data.Role exposing (Role)
import Shared.Utils.Theme exposing (Theme)
import String.Extra as String
import Time
import Wizard.Api.Models.BootstrapConfig exposing (BootstrapConfig)
import Wizard.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Wizard.Common.Flags as Flags
import Wizard.Common.GuideLinks as GuideLinks exposing (GuideLinks)
import Wizard.Data.Navigator exposing (Navigator)
import Wizard.Data.Session as Session exposing (Session)
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
    , websocketThrottleDelay : Float
    , config : BootstrapConfig
    , valid : Bool
    , currentTime : Time.Posix
    , timeZone : Time.Zone
    , navigator : Navigator
    , gaEnabled : Bool
    , cookieConsent : Bool
    , locale : Gettext.Locale
    , sessionExpiresSoonModalHidden : Bool
    , theme : Maybe Theme
    , guideLinks : GuideLinks
    , maxUploadFileSize : Int
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

        theme =
            if LookAndFeelConfig.isCustomTheme flags.config.lookAndFeel then
                Just <| LookAndFeelConfig.getTheme flags.config.lookAndFeel

            else
                Nothing
    in
    ( { route = Routes.NotFoundRoute
      , seed = Random.initialSeed flags.seed
      , session = Maybe.withDefault (Session.init flags.apiUrl) flags.session
      , invalidSession = invalidSession
      , key = key
      , apiUrl = flags.apiUrl
      , clientUrl = flags.clientUrl
      , websocketThrottleDelay = Maybe.withDefault 1000 flags.webSocketThrottleDelay
      , config = flags.config
      , valid = flags.success
      , currentTime = Time.millisToPosix 0
      , timeZone = Time.utc
      , navigator = flags.navigator
      , gaEnabled = flags.gaEnabled
      , cookieConsent = flags.cookieConsent
      , locale = flags.locale
      , sessionExpiresSoonModalHidden = False
      , theme = theme
      , guideLinks = GuideLinks.merge flags.guideLinks GuideLinks.default
      , maxUploadFileSize = Maybe.withDefault 100000000 flags.maxUploadFileSize
      }
    , flagsCmd
    )


toServerInfo : AppState -> ServerInfo
toServerInfo appState =
    { apiUrl = appState.apiUrl
    , token = String.toMaybe appState.session.token.token
    }


toAIAssistantServerInfo : AppState -> ServerInfo
toAIAssistantServerInfo appState =
    { apiUrl = getAIAssistantApiUrl appState
    , token = String.toMaybe appState.session.token.token
    }


getUserRole : AppState -> Maybe Role
getUserRole =
    Maybe.map .role << .user << .config


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

                Routes.ProjectsRoute (Wizard.Projects.Routes.DetailRoute _ (Wizard.Projects.Detail.ProjectDetailRoute.Questionnaire _ _)) ->
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


getAIAssistantApiUrl : AppState -> String
getAIAssistantApiUrl appState =
    String.replace "/wizard" "/ai-assistant" appState.apiUrl
