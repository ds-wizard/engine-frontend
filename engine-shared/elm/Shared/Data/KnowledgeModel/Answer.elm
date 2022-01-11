module Shared.Data.KnowledgeModel.Answer exposing
    ( Answer
    , addFollowUpUuid
    , decoder
    , removeFollowUpUuid
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.MetricMeasure as MetricMeasure exposing (MetricMeasure)


type alias Answer =
    { uuid : String
    , label : String
    , advice : Maybe String
    , metricMeasures : List MetricMeasure
    , followUpUuids : List String
    , annotations : List Annotation
    }


decoder : Decoder Answer
decoder =
    D.succeed Answer
        |> D.required "uuid" D.string
        |> D.required "label" D.string
        |> D.required "advice" (D.nullable D.string)
        |> D.required "metricMeasures" (D.list MetricMeasure.decoder)
        |> D.required "followUpUuids" (D.list D.string)
        |> D.required "annotations" (D.list Annotation.decoder)


addFollowUpUuid : String -> Answer -> Answer
addFollowUpUuid questionUuid answer =
    { answer | followUpUuids = answer.followUpUuids ++ [ questionUuid ] }


removeFollowUpUuid : String -> Answer -> Answer
removeFollowUpUuid questionUuid answer =
    { answer | followUpUuids = List.filter ((/=) questionUuid) answer.followUpUuids }
