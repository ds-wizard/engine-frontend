module ChartJS exposing
    ( ChartConfig
    , Data
    , DataSet
    , encodeChartConfig
    )

import Json.Encode as E exposing (Value)
import Json.Encode.Extra as E
import Round


type alias ChartConfig =
    { targetId : String
    , data : Data
    }


type alias Data =
    { labels : List String
    , datasets : List DataSet
    }


type alias DataSet =
    { label : String
    , borderColor : String
    , backgroundColor : String
    , pointBackgroundColor : String
    , data : List Float
    , stack : Maybe String
    }


encodeChartConfig : ChartConfig -> Value
encodeChartConfig config =
    E.object
        [ ( "targetId", E.string config.targetId )
        , ( "data", encodeData config.data )
        ]


encodeData : Data -> Value
encodeData data =
    E.object
        [ ( "labels", E.list E.string data.labels )
        , ( "datasets", E.list encodeDataSet data.datasets )
        ]


encodeDataSet : DataSet -> Value
encodeDataSet dataSet =
    E.object
        [ ( "label", E.string dataSet.label )
        , ( "borderColor", E.string dataSet.borderColor )
        , ( "backgroundColor", E.string dataSet.backgroundColor )
        , ( "pointBackgroundColor", E.string dataSet.pointBackgroundColor )
        , ( "data", E.list (E.float << Round.roundNum 2) dataSet.data )
        , ( "stack", E.maybe E.string dataSet.stack )
        ]
