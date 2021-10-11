module Shared.Data.KnowledgeModel.Answer exposing
    ( Answer
    , decoder
    , new
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.KnowledgeModel.MetricMeasure as MetricMeasure exposing (MetricMeasure)


type alias Answer =
    { uuid : String
    , label : String
    , advice : Maybe String
    , metricMeasures : List MetricMeasure
    , followUpUuids : List String
    , annotations : Dict String String
    }


new : String -> Answer
new uuid =
    { uuid = uuid
    , label = "New answer"
    , advice = Nothing
    , followUpUuids = []
    , metricMeasures = []
    , annotations = Dict.empty
    }


decoder : Decoder Answer
decoder =
    D.succeed Answer
        |> D.required "uuid" D.string
        |> D.required "label" D.string
        |> D.required "advice" (D.nullable D.string)
        |> D.required "metricMeasures" (D.list MetricMeasure.decoder)
        |> D.required "followUpUuids" (D.list D.string)
        |> D.required "annotations" (D.dict D.string)
