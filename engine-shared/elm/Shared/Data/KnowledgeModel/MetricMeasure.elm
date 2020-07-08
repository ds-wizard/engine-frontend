module Shared.Data.KnowledgeModel.MetricMeasure exposing
    ( MetricMeasure
    , decoder
    , encode
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
