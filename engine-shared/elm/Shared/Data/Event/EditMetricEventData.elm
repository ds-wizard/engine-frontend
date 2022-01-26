module Shared.Data.Event.EditMetricEventData exposing
    ( EditMetricEventData
    , apply
    , decoder
    , encode
    , init
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.Event.EventField as EventField exposing (EventField)
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)


type alias EditMetricEventData =
    { title : EventField String
    , abbreviation : EventField (Maybe String)
    , description : EventField (Maybe String)
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditMetricEventData
decoder =
    D.succeed EditMetricEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "abbreviation" (EventField.decoder (D.maybe D.string))
        |> D.required "description" (EventField.decoder (D.maybe D.string))
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditMetricEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditMetricEvent" )
    , ( "title", EventField.encode E.string data.title )
    , ( "abbreviation", EventField.encode (E.maybe E.string) data.abbreviation )
    , ( "description", EventField.encode (E.maybe E.string) data.description )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    ]


init : EditMetricEventData
init =
    { title = EventField.empty
    , abbreviation = EventField.empty
    , description = EventField.empty
    , annotations = EventField.empty
    }


apply : EditMetricEventData -> Metric -> Metric
apply eventData metric =
    { metric
        | title = EventField.getValueWithDefault eventData.title metric.title
        , abbreviation = EventField.getValueWithDefault eventData.abbreviation metric.abbreviation
        , description = EventField.getValueWithDefault eventData.description metric.description
        , annotations = EventField.getValueWithDefault eventData.annotations metric.annotations
    }
