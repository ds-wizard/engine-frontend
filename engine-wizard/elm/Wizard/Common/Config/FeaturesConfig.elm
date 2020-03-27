module Wizard.Common.Config.FeaturesConfig exposing
    ( FeaturesConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Common.Config.RegistryConfig as RegistryConfig exposing (RegistryConfig(..))
import Wizard.Common.Config.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)


type alias FeaturesConfig =
    { levels : SimpleFeatureConfig
    , publicQuestionnaire : SimpleFeatureConfig
    , questionnaireAccessibility : SimpleFeatureConfig
    , feedback : SimpleFeatureConfig
    , registry : RegistryConfig
    }


decoder : Decoder FeaturesConfig
decoder =
    D.succeed FeaturesConfig
        |> D.required "levels" SimpleFeatureConfig.decoder
        |> D.required "publicQuestionnaire" SimpleFeatureConfig.decoder
        |> D.required "questionnaireAccessibility" SimpleFeatureConfig.decoder
        |> D.required "feedback" SimpleFeatureConfig.decoder
        |> D.required "registry" RegistryConfig.decoder


default : FeaturesConfig
default =
    { levels = SimpleFeatureConfig.enabled
    , publicQuestionnaire = SimpleFeatureConfig.enabled
    , questionnaireAccessibility = SimpleFeatureConfig.enabled
    , feedback = SimpleFeatureConfig.enabled
    , registry = RegistryDisabled
    }
