module Wizard.Common.Config.QuestionnairesConfig exposing (QuestionnairesConfig, decoder, default)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Common.Config.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)


type alias QuestionnairesConfig =
    { questionnaireAccessibility : SimpleFeatureConfig
    , levels : SimpleFeatureConfig
    , feedback : SimpleFeatureConfig
    , publicQuestionnaire : SimpleFeatureConfig
    }


decoder : Decoder QuestionnairesConfig
decoder =
    D.succeed QuestionnairesConfig
        |> D.required "questionnaireAccessibility" SimpleFeatureConfig.decoder
        |> D.required "levels" SimpleFeatureConfig.decoder
        |> D.required "feedback" SimpleFeatureConfig.decoder
        |> D.required "publicQuestionnaire" SimpleFeatureConfig.decoder


default : QuestionnairesConfig
default =
    { questionnaireAccessibility = SimpleFeatureConfig.enabled
    , levels = SimpleFeatureConfig.enabled
    , feedback = SimpleFeatureConfig.enabled
    , publicQuestionnaire = SimpleFeatureConfig.enabled
    }
