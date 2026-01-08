module Wizard.Data.AppState exposing
    ( AppState
    , acceptCookies
    , anyPluginsAvailable
    , getClientUrlRoot
    , getPlugin
    , getPluginSettings
    , getPluginUserSettings
    , getPlugins
    , getPluginsByConnector
    , getUserRole
    , init
    , isFullscreen
    , sessionExpired
    , sessionExpiresSoon
    , sessionRemainingTime
    , setCurrentTime
    , setTimeZone
    , toAIAssistantServerInfo
    , toGuideLinkConfig
    , toServerInfo
    )

import Browser.Navigation as Navigation exposing (Key)
import Common.Api.Request exposing (ServerInfo)
import Common.Components.GuideLink as GuideLink
import Common.Data.Navigator exposing (Navigator)
import Common.Data.Role exposing (Role)
import Common.Utils.GuideLinks as GuideLinks exposing (GuideLinks)
import Common.Utils.Theme exposing (Theme)
import Dict
import Gettext
import Json.Decode as D exposing (Error(..))
import List.Extra as List
import Random exposing (Seed)
import String.Extra as String
import Time
import Uuid exposing (Uuid)
import Wizard.Api.Models.BootstrapConfig exposing (BootstrapConfig)
import Wizard.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Wizard.Data.Flags as Flags
import Wizard.Data.Session as Session exposing (Session)
import Wizard.Pages.KMEditor.Editor.KMEditorRoute
import Wizard.Pages.KMEditor.Routes
import Wizard.Pages.Projects.Detail.ProjectDetailRoute
import Wizard.Pages.Projects.Routes
import Wizard.Plugins.Plugin as Plugin exposing (Connectors, Plugin)
import Wizard.Plugins.PluginMetadata exposing (PluginMetadata)
import Wizard.Ports.Console as Console
import Wizard.Routes as Routes
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


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
    , urlCheckerUrl : Maybe String
    , newsUrl : Maybe String
    , pluginMetadata : List PluginMetadata
    , plugins : List Plugin
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
                    Console.consoleError (D.errorToString err)

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
      , guideLinks = GuideLinks.merge flags.guideLinks WizardGuideLinks.default
      , maxUploadFileSize = Maybe.withDefault 100000000 flags.maxUploadFileSize
      , urlCheckerUrl = flags.urlCheckerUrl
      , newsUrl = flags.newsUrl
      , pluginMetadata = flags.pluginMetadata
      , plugins = List.filter Plugin.isApiVersionSupported flags.plugins
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


toGuideLinkConfig : AppState -> (GuideLinks -> String) -> GuideLink.GuideLinkConfig
toGuideLinkConfig appState getLink =
    { guideLinks = appState.guideLinks
    , locale = appState.locale
    , getLink = getLink
    , serverInfo = toServerInfo appState
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
                Routes.KMEditorRoute (Wizard.Pages.KMEditor.Routes.EditorRoute _ (Wizard.Pages.KMEditor.Editor.KMEditorRoute.Edit _)) ->
                    True

                Routes.ProjectsRoute (Wizard.Pages.Projects.Routes.DetailRoute _ (Wizard.Pages.Projects.Detail.ProjectDetailRoute.Questionnaire _ _)) ->
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


anyPluginsAvailable : AppState -> Bool
anyPluginsAvailable appState =
    not (List.isEmpty appState.pluginMetadata)


getPlugins : AppState -> List Plugin
getPlugins appState =
    appState.plugins


getPluginsByConnector : AppState -> (Connectors -> Maybe (List a)) -> List ( Plugin, a )
getPluginsByConnector appState connectorGetter =
    appState.plugins
        |> List.filterMap
            (\plugin ->
                case connectorGetter plugin.connectors of
                    Just connector ->
                        Just (List.map (\connectorItem -> ( plugin, connectorItem )) connector)

                    Nothing ->
                        Nothing
            )
        |> List.concat


getPlugin : AppState -> Uuid -> Maybe Plugin
getPlugin appState pluginUuid =
    List.find ((==) pluginUuid << .uuid) appState.plugins


getPluginUserSettings : AppState -> Uuid -> String
getPluginUserSettings appState pluginUuid =
    appState.config.user
        |> Maybe.map .pluginSettings
        |> Maybe.andThen (Dict.get (Uuid.toString pluginUuid))
        |> Maybe.withDefault ""


getPluginSettings : AppState -> Uuid -> String
getPluginSettings appState pluginUuid =
    Dict.get (Uuid.toString pluginUuid) appState.config.pluginSettings
        |> Maybe.withDefault ""
