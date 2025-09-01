module Wizard.Api.Models.Event.AddMetricEventData exposing
    ( AddMetricEventData
    , decoder
    , encode
    , init
    , toMetric
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.Metric exposing (Metric)


type alias AddMetricEventData =
    { title : String
    , abbreviation : Maybe String
    , description : Maybe String
    , annotations : List Annotation
    }


decoder : Decoder AddMetricEventData
decoder =
    D.succeed AddMetricEventData
        |> D.required "title" D.string
        |> D.required "abbreviation" (D.maybe D.string)
        |> D.required "description" (D.maybe D.string)
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddMetricEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddMetricEvent" )
    , ( "title", E.string data.title )
    , ( "abbreviation", E.maybe E.string data.abbreviation )
    , ( "description", E.maybe E.string data.description )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]


init : AddMetricEventData
init =
    { title = ""
    , abbreviation = Nothing
    , description = Nothing
    , annotations = []
    }


toMetric : String -> AddMetricEventData -> Metric
toMetric uuid data =
    { uuid = uuid
    , title = data.title
    , abbreviation = data.abbreviation
    , description = data.description
    , annotations = data.annotations
    }
