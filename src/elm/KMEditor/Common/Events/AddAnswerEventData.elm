module KMEditor.Common.Events.AddAnswerEventData exposing
    ( AddAnswerEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import KMEditor.Common.KnowledgeModel.MetricMeasure as MetricMeasure exposing (MetricMeasure)


type alias AddAnswerEventData =
    { label : String
    , advice : Maybe String
    , metricMeasures : List MetricMeasure
    }


decoder : Decoder AddAnswerEventData
decoder =
    D.succeed AddAnswerEventData
        |> D.required "label" D.string
        |> D.required "advice" (D.nullable D.string)
        |> D.required "metricMeasures" (D.list MetricMeasure.decoder)


encode : AddAnswerEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddAnswerEvent" )
    , ( "label", E.string data.label )
    , ( "advice", E.maybe E.string data.advice )
    , ( "metricMeasures", E.list MetricMeasure.encode data.metricMeasures )
    ]
