module Wizard.Common.AppState exposing
    ( AppState
    , getDashboardWidgets
    , init
    , setCurrentTime
    )

import Browser.Navigation as Navigation exposing (Key)
import Dict
import Json.Decode as D exposing (Decoder, Error(..))
import Random exposing (Seed)
import Shared.Auth.Session as Session exposing (Session)
import Shared.Data.BootstrapConfig exposing (BootstrapConfig)
import Shared.Data.BootstrapConfig.DashboardConfig.DashboardWidget exposing (DashboardWidget(..))
import Shared.Provisioning as Provisioning exposing (Provisioning)
import Time
import Wizard.Common.Flags as Flags
import Wizard.Common.Provisioning.DefaultIconSet as DefaultIconSet
import Wizard.Common.Provisioning.DefaultLocale as DefaultLocale
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
    }


init : D.Value -> Navigation.Key -> AppState
init flagsValue key =
    let
        flagsResult =
            D.decodeValue Flags.decoder flagsValue

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
    { route = Routes.NotFoundRoute
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
    }


setCurrentTime : AppState -> Time.Posix -> AppState
setCurrentTime appState time =
    { appState | currentTime = time }


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
