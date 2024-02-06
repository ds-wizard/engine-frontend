module Registry2.Data.AppState exposing
    ( AppState
    , default
    , init
    , setTimeZone
    )

import Browser.Navigation as Navigation
import Gettext
import Json.Decode as D
import Registry2.Data.Flags as Flags
import Registry2.Routes as Routes
import Time


type alias AppState =
    { route : Routes.Route
    , key : Navigation.Key
    , apiUrl : String
    , locale : Gettext.Locale
    , timeZone : Time.Zone
    }


default : Navigation.Key -> AppState
default key =
    { route = Routes.NotFound
    , key = key
    , apiUrl = ""
    , locale = Gettext.defaultLocale
    , timeZone = Time.utc
    }


init : D.Value -> Navigation.Key -> Maybe AppState
init flagsValue key =
    case D.decodeValue Flags.decoder flagsValue of
        Ok flags ->
            Just
                { route = Routes.NotFound
                , key = key
                , apiUrl = flags.apiUrl
                , locale = Gettext.defaultLocale
                , timeZone = Time.utc
                }

        Err _ ->
            Nothing


setTimeZone : Time.Zone -> AppState -> AppState
setTimeZone timeZone appState =
    { appState | timeZone = timeZone }
