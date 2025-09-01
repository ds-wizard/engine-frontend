module Wizard.Api.Models.Event.AddAnswerEventData exposing
    ( AddAnswerEventData
    , decoder
    , encode
    , init
    , toAnswer
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Wizard.Api.Models.KnowledgeModel.MetricMeasure as MetricMeasure exposing (MetricMeasure)


type alias AddAnswerEventData =
    { label : String
    , advice : Maybe String
    , metricMeasures : List MetricMeasure
    , annotations : List Annotation
    }


decoder : Decoder AddAnswerEventData
decoder =
    D.succeed AddAnswerEventData
        |> D.required "label" D.string
        |> D.required "advice" (D.nullable D.string)
        |> D.required "metricMeasures" (D.list MetricMeasure.decoder)
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddAnswerEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddAnswerEvent" )
    , ( "label", E.string data.label )
    , ( "advice", E.maybe E.string data.advice )
    , ( "metricMeasures", E.list MetricMeasure.encode data.metricMeasures )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]


init : AddAnswerEventData
init =
    { label = ""
    , advice = Nothing
    , metricMeasures = []
    , annotations = []
    }


toAnswer : String -> AddAnswerEventData -> Answer
toAnswer uuid data =
    { uuid = uuid
    , label = data.label
    , advice = data.advice
    , metricMeasures = data.metricMeasures
    , followUpUuids = []
    , annotations = data.annotations
    }
