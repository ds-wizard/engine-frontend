module Shared.Data.Event.EditAnswerEventData exposing
    ( EditAnswerEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.Event.EventField as EventField exposing (EventField)
import Shared.Data.KnowledgeModel.MetricMeasure as MetricMeasure exposing (MetricMeasure)


type alias EditAnswerEventData =
    { label : EventField String
    , advice : EventField (Maybe String)
    , metricMeasures : EventField (List MetricMeasure)
    , followUpUuids : EventField (List String)
    }


decoder : Decoder EditAnswerEventData
decoder =
    D.succeed EditAnswerEventData
        |> D.required "label" (EventField.decoder D.string)
        |> D.required "advice" (EventField.decoder (D.nullable D.string))
        |> D.required "metricMeasures" (EventField.decoder (D.list MetricMeasure.decoder))
        |> D.required "followUpUuids" (EventField.decoder (D.list D.string))


encode : EditAnswerEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditAnswerEvent" )
    , ( "label", EventField.encode E.string data.label )
    , ( "advice", EventField.encode (E.maybe E.string) data.advice )
    , ( "metricMeasures", EventField.encode (E.list MetricMeasure.encode) data.metricMeasures )
    , ( "followUpUuids", EventField.encode (E.list E.string) data.followUpUuids )
    ]
