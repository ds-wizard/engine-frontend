module WizardResearch.Common.AppState exposing
    ( AppState
    , authenticated
    , init
    , setSession
    )

import Json.Decode as D
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Result.Extra as Result
import Shared.Api exposing (ApiConfig)
import Shared.Data.BootstrapConfig exposing (BootstrapConfig)
import Shared.Elemental.Theme as Theme exposing (Theme)
import Shared.Provisioning as Provisioning exposing (Provisioning)
import Shared.Setters exposing (setToken)
import WizardResearch.Common.Flags as Flags
import WizardResearch.Common.Provisioning.DefaultIconSet as DefaultIconSet
import WizardResearch.Common.Provisioning.DefaultLocale as DefaultLocale
import WizardResearch.Common.Session exposing (Session)


type alias AppState =
    { seed : Seed
    , provisioning : Provisioning
    , apiConfig : ApiConfig
    , config : BootstrapConfig
    , session : Maybe Session
    , configurationError : Bool
    , theme : Theme
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
    , apiConfig =
        { apiUrl = flags.apiUrl
        , token = Maybe.unwrap "" .token flags.session
        }
    , config = flags.config
    , session = flags.session
    , configurationError = configurationError
    , theme = Theme.default
    }


setSession : Maybe Session -> AppState -> AppState
setSession mbSession appState =
    { appState
        | session = mbSession
        , apiConfig = setToken (Maybe.unwrap "" .token mbSession) appState.apiConfig
    }


authenticated : AppState -> Bool
authenticated appState =
    Maybe.isJust appState.session
