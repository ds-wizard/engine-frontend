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
import Shared.Provisioning as Provisioning exposing (Provisioning)
import Time
import Wizard.Common.Config exposing (Config)
import Wizard.Common.Config.DashboardWidget exposing (DashboardWidget(..))
import Wizard.Common.Flags as Flags
import Wizard.Common.JwtToken as JwtToken exposing (JwtToken)
import Wizard.Common.Provisioning.DefaultIconSet as DefaultIconSet
import Wizard.Common.Provisioning.DefaultLocale as DefaultLocale
import Wizard.Common.Session as Session exposing (Session)
import Wizard.Routes as Routes


type alias AppState =
    { route : Routes.Route
    , seed : Seed
    , session : Session
    , invalidSession : Bool
    , jwt : Maybe JwtToken
    , key : Key
    , apiUrl : String
    , config : Config
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
    , jwt = Maybe.andThen (.token >> JwtToken.parse) flags.session
    , key = key
    , apiUrl = flags.apiUrl
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
    Dict.get role appState.config.client.dashboard
        |> Maybe.withDefault [ WelcomeDashboardWidget ]
