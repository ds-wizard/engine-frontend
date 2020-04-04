module WizardResearch.Common.AppState exposing
    ( AppState
    , authenticated
    , init
    , setSession
    )

import Json.Decode as D
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Shared.Api exposing (ApiConfig)
import Shared.Data.BootstrapConfig exposing (BootstrapConfig)
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
    }


init : D.Value -> AppState
init flagsValue =
    let
        flags =
            Result.withDefault Flags.default <|
                D.decodeValue Flags.decoder flagsValue

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
        , token = ""
        }
    , config = flags.config
    , session = flags.session
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
