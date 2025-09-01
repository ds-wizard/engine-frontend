module Registry.Data.AppState exposing
    ( AppState
    , default
    , getAppTitle
    , getOrganizationId
    , init
    , setSession
    , setTimeZone
    , toServerInfo
    )

import Browser.Navigation as Navigation
import Gettext
import Json.Decode as D
import Registry.Api.Models.BootstrapConfig as BootstrapConfig exposing (BootstrapConfig)
import Registry.Data.Flags as Flags
import Registry.Data.Session exposing (Session)
import Registry.Routes as Routes
import Shared.Api.Request exposing (ServerInfo)
import Time


type alias AppState =
    { route : Routes.Route
    , config : BootstrapConfig
    , key : Navigation.Key
    , appTitle : Maybe String
    , apiUrl : String
    , locale : Gettext.Locale
    , timeZone : Time.Zone
    , session : Maybe Session
    }


default : Navigation.Key -> AppState
default key =
    { route = Routes.NotFound
    , config = BootstrapConfig.default
    , key = key
    , appTitle = Nothing
    , apiUrl = ""
    , locale = Gettext.defaultLocale
    , timeZone = Time.utc
    , session = Nothing
    }


init : D.Value -> Navigation.Key -> Maybe AppState
init flagsValue key =
    case D.decodeValue Flags.decoder flagsValue of
        Ok flags ->
            Just
                { route = Routes.NotFound
                , config = flags.config
                , key = key
                , appTitle = flags.appTitle
                , apiUrl = flags.apiUrl
                , locale = Gettext.defaultLocale
                , timeZone = Time.utc
                , session = flags.session
                }

        Err _ ->
            Nothing


toServerInfo : AppState -> ServerInfo
toServerInfo appState =
    { apiUrl = appState.apiUrl
    , token = Maybe.map .token appState.session
    }


setTimeZone : Time.Zone -> AppState -> AppState
setTimeZone timeZone appState =
    { appState | timeZone = timeZone }


setSession : Maybe Session -> AppState -> AppState
setSession mbSession appState =
    { appState | session = mbSession }


getOrganizationId : AppState -> Maybe String
getOrganizationId appState =
    Maybe.map .organizationId appState.session


getAppTitle : AppState -> String
getAppTitle appState =
    Maybe.withDefault "DSW Registry" appState.appTitle
