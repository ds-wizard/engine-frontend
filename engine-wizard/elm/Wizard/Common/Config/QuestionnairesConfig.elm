module Wizard.Common.Config.QuestionnairesConfig exposing
    ( QuestionnairesConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Common.Config.Partials.QuestionnaireVisibilityConfig as QuestionnaireVisibilityConfig exposing (QuestionnaireVisibilityConfig)
import Wizard.Common.Config.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)


type alias QuestionnairesConfig =
    { questionnaireVisibility : QuestionnaireVisibilityConfig
    , levels : SimpleFeatureConfig
    , feedback : SimpleFeatureConfig
    , summaryReport : SimpleFeatureConfig
    }


decoder : Decoder QuestionnairesConfig
decoder =
    D.succeed QuestionnairesConfig
        |> D.required "questionnaireVisibility" QuestionnaireVisibilityConfig.decoder
        |> D.required "levels" SimpleFeatureConfig.decoder
        |> D.required "feedback" SimpleFeatureConfig.decoder
        |> D.required "summaryReport" SimpleFeatureConfig.decoder


default : QuestionnairesConfig
default =
    { questionnaireVisibility = QuestionnaireVisibilityConfig.default
    , levels = SimpleFeatureConfig.enabled
    , feedback = SimpleFeatureConfig.enabled
    , summaryReport = SimpleFeatureConfig.enabled
    }
