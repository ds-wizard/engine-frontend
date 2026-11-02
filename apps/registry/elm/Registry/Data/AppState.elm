module Registry.Data.AppState exposing
    ( AppState
    , init
    , isAdmin
    , isSignedIn
    , toServerInfo
    )

{-| What every page of the registry may read: where the API is, the session,
what the registry says about itself, the person signed in, the language, the
clock.

The composites of `Common` take the fields they read (`Common.Data.Context`),
so this record is passed to them as it is.

-}

import Browser.Navigation exposing (Key)
import Common.Api.Request exposing (ServerInfo)
import Common.Data.Navigator as Navigator exposing (Navigator)
import Common.Data.Session as Session exposing (Session)
import Gettext
import Json.Decode as D
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe
import Registry.Api.Models.BootstrapConfig as BootstrapConfig exposing (BootstrapConfig)
import Registry.Api.Models.User as User exposing (User)
import String.Extra as String
import Time
import Ui.Theme as Theme


type alias AppState =
    { key : Key
    , apiUrl : String
    , session : Session
    , config : BootstrapConfig
    , valid : Bool
    , locale : Gettext.Locale
    , navigator : Navigator
    , currentTime : Time.Posix
    , timeZone : Time.Zone
    , uiMode : Theme.Mode
    , sideNavWidth : Maybe Float
    , user : Maybe User
    }


type alias Flags =
    { apiUrl : String
    , config : BootstrapConfig
    , session : Maybe Session
    , navigator : Navigator
    , sideNavWidth : Maybe Float
    , uiMode : Maybe String
    }


flagsDecoder : D.Decoder Flags
flagsDecoder =
    D.succeed Flags
        |> D.required "apiUrl" D.string
        |> D.required "config" BootstrapConfig.decoder
        |> D.required "session" (D.nullable Session.decoder)
        |> D.required "navigator" Navigator.decoder
        |> D.optional "sideNavWidth" (D.maybe D.float) Nothing
        |> D.optional "uiMode" (D.maybe D.string) Nothing


{-| Flags that cannot be read leave the registry `valid = False`: it says it
cannot start rather than starting with a configuration nobody gave it.
-}
init : D.Value -> Key -> AppState
init flagsValue key =
    let
        result =
            D.decodeValue flagsDecoder flagsValue
    in
    case result of
        Ok flags ->
            { key = key
            , apiUrl = flags.apiUrl
            , session = Maybe.withDefault (Session.init flags.apiUrl) flags.session
            , config = flags.config
            , valid = True
            , locale = Gettext.defaultLocale
            , navigator = flags.navigator
            , currentTime = Time.millisToPosix 0
            , timeZone = Time.utc
            , uiMode = modeFromFlag flags.uiMode
            , sideNavWidth = flags.sideNavWidth
            , user = Nothing
            }

        Err _ ->
            let
                apiUrl =
                    Result.withDefault "" (D.decodeValue (D.field "apiUrl" D.string) flagsValue)
            in
            { key = key
            , apiUrl = apiUrl
            , session = Session.init apiUrl
            , config = BootstrapConfig.default
            , valid = False
            , locale = Gettext.defaultLocale
            , navigator = Navigator.default
            , currentTime = Time.millisToPosix 0
            , timeZone = Time.utc
            , uiMode = Theme.System
            , sideNavWidth = Nothing
            , user = Nothing
            }


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


isSignedIn : AppState -> Bool
isSignedIn appState =
    Session.exists appState.session


isAdmin : AppState -> Bool
isAdmin appState =
    Maybe.unwrap False User.isAdmin appState.user
