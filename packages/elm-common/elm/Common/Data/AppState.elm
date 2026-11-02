module Common.Data.AppState exposing
    ( AddUserLink
    , AppState
    , ProjectListLink
    , UserSettingsLink
    , anyPluginsAvailable
    , getClientUrlRoot
    , getPlugin
    , getPluginSettings
    , getPluginUser
    , getPluginUserSettings
    , getPlugins
    , getPluginsByConnector
    , init
    , sessionExpired
    , sessionExpiresSoon
    , sessionRemainingTime
    , setCurrentTime
    , setTimeZone
    , toServerInfo
    , userHasPerm
    , userPermissions
    )

import Browser.Navigation as Navigation exposing (Key)
import Common.Api.Models.BootstrapConfig exposing (BootstrapConfig)
import Common.Api.Models.BootstrapConfig.UserConfig as UserConfig
import Common.Api.Models.RolePermission exposing (RolePermission)
import Common.Api.Params as Params exposing (RequestParams)
import Common.Api.Request exposing (ServerInfo)
import Common.Data.ClientDefaults as ClientDefaults exposing (ClientDefaults)
import Common.Data.Extensions as Extensions exposing (Extensions)
import Common.Data.Flags as Flags
import Common.Data.Navigator exposing (Navigator)
import Common.Data.RowPolicy exposing (RowBadge)
import Common.Data.Session as Session exposing (Session)
import Common.Plugins.Plugin as Plugin exposing (Connectors, Plugin)
import Common.Plugins.PluginMetadata exposing (PluginMetadata)
import Common.Ports.Console as Console
import Common.Routes as Routes
import Common.Utils.GuideLinks exposing (GuideLinks)
import Dict
import Gettext
import Json.Decode as D exposing (Error(..))
import Json.Encode as E
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Seed)
import String.Extra as String
import Time
import Ui.Theme as Theme
import Uuid exposing (Uuid)


type alias AppState =
    { seed : Seed
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
    , locale : Gettext.Locale
    , sessionExpiresSoonModalHidden : Bool
    , guideLinks : GuideLinks
    , maxUploadFileSize : Int
    , urlCheckerUrl : Maybe String
    , newsUrl : Maybe String
    , pluginMetadata : List PluginMetadata
    , plugins : List Plugin
    , sideNavWidth : Maybe Float
    , uiMode : Theme.Mode

    -- How this application addresses a page, and what it adds to a request.
    -- Each application supplies its own; the shared pages only ask.
    , toUrl : Routes.Route -> String
    , toUrlIn : Maybe Uuid -> Routes.Route -> String
    , reachable : Routes.Route -> Bool
    , requestParams : RequestParams

    -- Whether the user is a member of the workspace they are in. False only
    -- where an application lets someone into a workspace through their role in
    -- the organization; what they create there is then not theirs as a
    -- member's would be.
    , workspaceMember : Bool

    -- The workspace the reader is in, where an application has workspaces:
    -- what is given to somebody there is given to one of its members.
    , workspaceUuid : Maybe Uuid

    -- The badge saying whose a knowledge model or a document template is,
    -- asked with its `workspaceUuid` where one is offered or shown outside its
    -- own listing — in a picker, in a project's settings — so two of the same
    -- id can be told apart. The listings themselves have `RowPolicy.badge`.
    , ownerBadge : Maybe Uuid -> Maybe RowBadge

    -- What the client falls back on where the server says nothing.
    , clientDefaults : ClientDefaults
    , addUserLinks : List AddUserLink
    , userSettingsLinks : List UserSettingsLink

    -- What the client adds to the shared projects list where the reader is,
    -- beside "Create" — an import, say. Set again on every route.
    , projectListLinks : List ProjectListLink

    -- What the client adds to the shared pages (`Common.Data.Extensions`).
    , extensions : Extensions
    }


{-| A way to add users that a client offers on the shared users list instead
of "Create" — an invitation, an import. Data only: the page behind `url` is the
client's own. `icon` is a Font Awesome solid name. `primary` marks the list's
main action, drawn last as "Create" would be; the others are drawn before it as
secondary ones, as on every list. Empty, the list offers "Create".
-}
type alias AddUserLink =
    { label : String
    , url : String
    , icon : String
    , testId : String
    , primary : Bool
    }


{-| A page a client offers on the shared projects list, beside "Create" — an
import, say. Data only: the page behind `url` is the client's own, and so is
the decision whether to offer it there. `icon` is a Font Awesome solid name.
-}
type alias ProjectListLink =
    { label : String
    , url : String
    , icon : String
    , testId : String
    }


{-| A section a client adds to the navigation of the shared user settings,
after the shared ones. Data only: `path` follows `/users/edit/<user>/`, and the
page there is the client's own. `ownOnly` sections are shown in one's own
settings and not in somebody else's.
-}
type alias UserSettingsLink =
    { id : String
    , label : String
    , path : String
    , ownOnly : Bool
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
    in
    ( { seed = Random.initialSeed flags.seed
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
      , locale = flags.locale
      , sessionExpiresSoonModalHidden = False
      , guideLinks = flags.guideLinks
      , maxUploadFileSize = Maybe.withDefault 100000000 flags.maxUploadFileSize
      , urlCheckerUrl = flags.urlCheckerUrl
      , newsUrl = flags.newsUrl
      , pluginMetadata = flags.pluginMetadata
      , plugins = List.filter Plugin.isApiVersionSupported flags.plugins
      , sideNavWidth = flags.sideNavWidth
      , uiMode = modeFromFlag flags.uiMode
      , toUrl = Routes.toPath
      , toUrlIn = \_ -> Routes.toPath
      , reachable = always True
      , requestParams = Params.none
      , workspaceMember = True
      , workspaceUuid = Nothing
      , ownerBadge = always Nothing
      , clientDefaults = ClientDefaults.none
      , addUserLinks = []
      , userSettingsLinks = []
      , projectListLinks = []
      , extensions = Extensions.none
      }
    , flagsCmd
    )


{-| The appearance the pre-paint script in `index.ejs` has already applied, so
the switch in the account menu starts on the choice that is actually showing.
Anything unrecognized is the system's, which is also what an absent value means.
-}
modeFromFlag : Maybe String -> Theme.Mode
modeFromFlag mbMode =
    case mbMode of
        Just "light" ->
            Theme.Light

        Just "dark" ->
            Theme.Dark

        _ ->
            Theme.System


toServerInfo : AppState -> ServerInfo
toServerInfo appState =
    { apiUrl = appState.apiUrl
    , token = String.toMaybe appState.session.token.token
    }


userHasPerm : RolePermission -> AppState -> Bool
userHasPerm perm appState =
    Maybe.unwrap False (UserConfig.hasPerm perm) appState.config.user


userPermissions : AppState -> List RolePermission
userPermissions appState =
    Maybe.unwrap [] .permissions appState.config.user


{-| The client's own address, without a trailing slash, for links given to
somebody else: a route's URL starts with one.
-}
getClientUrlRoot : AppState -> String
getClientUrlRoot appState =
    if String.endsWith "/" appState.clientUrl then
        String.dropRight 1 appState.clientUrl

    else
        appState.clientUrl


setCurrentTime : AppState -> Time.Posix -> AppState
setCurrentTime appState time =
    { appState | currentTime = time }


setTimeZone : AppState -> Time.Zone -> AppState
setTimeZone appState timeZone =
    { appState | timeZone = timeZone }


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


anyPluginsAvailable : AppState -> Bool
anyPluginsAvailable appState =
    not (List.isEmpty appState.pluginMetadata)


{-| The plugins there are to use: every one that was loaded, less those
switched off where the user is (`config.disabledPlugins`). The settings pages,
which list plugins in order to switch them, read `appState.plugins`.
-}
getPlugins : { a | plugins : List Plugin, config : BootstrapConfig } -> List Plugin
getPlugins appState =
    List.filter (\plugin -> List.notMember (Uuid.toString plugin.uuid) appState.config.disabledPlugins) appState.plugins


getPluginsByConnector : AppState -> (Connectors -> Maybe (List a)) -> List ( Plugin, a )
getPluginsByConnector appState connectorGetter =
    getPlugins appState
        |> List.filterMap
            (\plugin ->
                case connectorGetter plugin.connectors of
                    Just connector ->
                        Just (List.map (\connectorItem -> ( plugin, connectorItem )) connector)

                    Nothing ->
                        Nothing
            )
        |> List.concat


getPlugin : { a | plugins : List Plugin, config : BootstrapConfig } -> Uuid -> Maybe Plugin
getPlugin appState pluginUuid =
    List.find ((==) pluginUuid << .uuid) (getPlugins appState)


getPluginSettings : { a | config : BootstrapConfig } -> Uuid -> String
getPluginSettings appState pluginUuid =
    Dict.get (Uuid.toString pluginUuid) appState.config.pluginSettings
        |> Maybe.withDefault ""


getPluginUserSettings : AppState -> Uuid -> String
getPluginUserSettings appState pluginUuid =
    appState.config.user
        |> Maybe.map .pluginSettings
        |> Maybe.andThen (Dict.get (Uuid.toString pluginUuid))
        |> Maybe.withDefault ""


getPluginUser : AppState -> String
getPluginUser appState =
    appState.config.user
        |> Maybe.map (E.encode 0 << UserConfig.encode)
        |> Maybe.withDefault ""
