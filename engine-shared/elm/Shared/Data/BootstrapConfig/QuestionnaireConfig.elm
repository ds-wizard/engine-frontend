module Shared.Data.BootstrapConfig.QuestionnaireConfig exposing
    ( QuestionnaireConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.BootstrapConfig.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)
import Shared.Data.BootstrapConfig.QuestionnaireConfig.QuestionnaireVisibilityConfig as QuestionnaireVisibilityConfig exposing (QuestionnaireVisibilityConfig)


type alias QuestionnaireConfig =
    { questionnaireVisibility : QuestionnaireVisibilityConfig
    , levels : SimpleFeatureConfig
    , feedback : SimpleFeatureConfig
    , summaryReport : SimpleFeatureConfig
    }


decoder : Decoder QuestionnaireConfig
decoder =
    D.succeed QuestionnaireConfig
        |> D.required "questionnaireVisibility" QuestionnaireVisibilityConfig.decoder
        |> D.required "levels" SimpleFeatureConfig.decoder
        |> D.required "feedback" SimpleFeatureConfig.decoder
        |> D.required "summaryReport" SimpleFeatureConfig.decoder


default : QuestionnaireConfig
default =
    { questionnaireVisibility = QuestionnaireVisibilityConfig.default
    , levels = SimpleFeatureConfig.init True
    , feedback = SimpleFeatureConfig.init True
    , summaryReport = SimpleFeatureConfig.init True
    }
