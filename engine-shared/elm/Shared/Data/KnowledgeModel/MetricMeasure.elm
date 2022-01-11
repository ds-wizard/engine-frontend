module Shared.Data.KnowledgeModel.MetricMeasure exposing
    ( MetricMeasure
    , decoder
    , encode
    , init
    , setMeasure
    , setWeight
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias MetricMeasure =
    { metricUuid : String
    , measure : Float
    , weight : Float
    }


decoder : Decoder MetricMeasure
decoder =
    D.succeed MetricMeasure
        |> D.required "metricUuid" D.string
        |> D.required "measure" D.float
        |> D.required "weight" D.float


encode : MetricMeasure -> E.Value
encode metricMeasure =
    E.object
        [ ( "metricUuid", E.string metricMeasure.metricUuid )
        , ( "measure", E.float metricMeasure.measure )
        , ( "weight", E.float metricMeasure.weight )
        ]


init : String -> MetricMeasure
init metricUuid =
    { metricUuid = metricUuid
    , measure = 1
    , weight = 1
    }


setMeasure : Float -> MetricMeasure -> MetricMeasure
setMeasure measure metricMeasure =
    { metricMeasure | measure = measure }


setWeight : Float -> MetricMeasure -> MetricMeasure
setWeight weight metricMeasure =
    { metricMeasure | weight = weight }
