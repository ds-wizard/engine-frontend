module Wizard.Common.Config.QuestionnairesConfig exposing (QuestionnairesConfig, decoder, default)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Common.Config.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)


type alias QuestionnairesConfig =
    { questionnaireVisibility : SimpleFeatureConfig
    , levels : SimpleFeatureConfig
    , feedback : SimpleFeatureConfig
    }


decoder : Decoder QuestionnairesConfig
decoder =
    D.succeed QuestionnairesConfig
        |> D.required "questionnaireVisibility" SimpleFeatureConfig.decoder
        |> D.required "levels" SimpleFeatureConfig.decoder
        |> D.required "feedback" SimpleFeatureConfig.decoder


default : QuestionnairesConfig
default =
    { questionnaireVisibility = SimpleFeatureConfig.enabled
    , levels = SimpleFeatureConfig.enabled
    , feedback = SimpleFeatureConfig.enabled
    }
