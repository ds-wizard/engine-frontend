module Shared.Data.Event.EditAnswerEventData exposing
    ( EditAnswerEventData
    , apply
    , decoder
    , encode
    , init
    , squash
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.Event.EventField as EventField exposing (EventField)
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Answer exposing (Answer)
import Shared.Data.KnowledgeModel.MetricMeasure as MetricMeasure exposing (MetricMeasure)


type alias EditAnswerEventData =
    { label : EventField String
    , advice : EventField (Maybe String)
    , metricMeasures : EventField (List MetricMeasure)
    , followUpUuids : EventField (List String)
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditAnswerEventData
decoder =
    D.succeed EditAnswerEventData
        |> D.required "label" (EventField.decoder D.string)
        |> D.required "advice" (EventField.decoder (D.nullable D.string))
        |> D.required "metricMeasures" (EventField.decoder (D.list MetricMeasure.decoder))
        |> D.required "followUpUuids" (EventField.decoder (D.list D.string))
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditAnswerEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditAnswerEvent" )
    , ( "label", EventField.encode E.string data.label )
    , ( "advice", EventField.encode (E.maybe E.string) data.advice )
    , ( "metricMeasures", EventField.encode (E.list MetricMeasure.encode) data.metricMeasures )
    , ( "followUpUuids", EventField.encode (E.list E.string) data.followUpUuids )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    ]


init : EditAnswerEventData
init =
    { label = EventField.empty
    , advice = EventField.empty
    , metricMeasures = EventField.empty
    , followUpUuids = EventField.empty
    , annotations = EventField.empty
    }


apply : EditAnswerEventData -> Answer -> Answer
apply eventData answer =
    { answer
        | label = EventField.getValueWithDefault eventData.label answer.label
        , advice = EventField.getValueWithDefault eventData.advice answer.advice
        , metricMeasures = EventField.getValueWithDefault eventData.metricMeasures answer.metricMeasures
        , followUpUuids = EventField.applyChildren eventData.followUpUuids answer.followUpUuids
        , annotations = EventField.getValueWithDefault eventData.annotations answer.annotations
    }


squash : EditAnswerEventData -> EditAnswerEventData -> EditAnswerEventData
squash oldData newData =
    { label = EventField.squash oldData.label newData.label
    , advice = EventField.squash oldData.advice newData.advice
    , metricMeasures = EventField.squash oldData.metricMeasures newData.metricMeasures
    , followUpUuids = EventField.squash oldData.followUpUuids newData.followUpUuids
    , annotations = EventField.squash oldData.annotations newData.annotations
    }
