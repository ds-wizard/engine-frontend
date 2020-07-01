module WizardResearch.Common.AppState exposing
    ( AppState
    , authenticated
    , init
    , setCurrentTime
    , setSession
    )

import Json.Decode as D
import Random exposing (Seed)
import Result.Extra as Result
import Shared.Auth.Session as Session exposing (Session)
import Shared.Data.BootstrapConfig exposing (BootstrapConfig)
import Shared.Elemental.Theme as Theme exposing (Theme)
import Shared.Provisioning as Provisioning exposing (Provisioning)
import Time
import WizardResearch.Common.Flags as Flags
import WizardResearch.Common.Provisioning.DefaultIconSet as DefaultIconSet
import WizardResearch.Common.Provisioning.DefaultLocale as DefaultLocale


type alias AppState =
    { seed : Seed
    , provisioning : Provisioning
    , apiUrl : String
    , config : BootstrapConfig
    , session : Session
    , configurationError : Bool
    , theme : Theme
    , currentTime : Time.Posix
    }


init : D.Value -> AppState
init flagsValue =
    let
        flagsResult =
            D.decodeValue Flags.decoder flagsValue

        flags =
            Result.withDefault Flags.default flagsResult

        configurationError =
            Result.isErr flagsResult

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
    { seed = Random.initialSeed flags.seed
    , provisioning = provisioning
    , apiUrl = flags.apiUrl
    , config = flags.config
    , session = Maybe.withDefault Session.init flags.session
    , configurationError = configurationError
    , theme = Theme.default
    , currentTime = Time.millisToPosix 0
    }


setSession : Maybe Session -> AppState -> AppState
setSession mbSession appState =
    { appState | session = Maybe.withDefault Session.init mbSession }


setCurrentTime : Time.Posix -> AppState -> AppState
setCurrentTime time appState =
    { appState | currentTime = time }


authenticated : AppState -> Bool
authenticated appState =
    Session.exists appState.session
