module WizardResearch.Common.AppState exposing (..)

import Browser.Navigation as Navigation exposing (Key)
import Json.Decode as D
import Random exposing (Seed)
import Shared.Provisioning as Provisioning exposing (Provisioning)
import WizardResearch.Common.Flags as Flags
import WizardResearch.Common.Provisioning.DefaultIconSet as DefaultIconSet
import WizardResearch.Common.Provisioning.DefaultLocale as DefaultLocale


type alias AppState =
    { seed : Seed
    , key : Key
    , apiUrl : String
    , provisioning : Provisioning
    }


init : D.Value -> Navigation.Key -> AppState
init flagsValue key =
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
    , key = key
    , apiUrl = flags.apiUrl
    , provisioning = provisioning
    }
