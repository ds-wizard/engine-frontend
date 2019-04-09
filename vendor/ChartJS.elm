module ChartJS exposing
    ( ChartConfig
    , Data
    , DataSet
    , encodeChartConfig
    )

import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra exposing (maybe)


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
    Encode.object
        [ ( "targetId", Encode.string config.targetId )
        , ( "data", encodeData config.data )
        ]


encodeData : Data -> Value
encodeData data =
    Encode.object
        [ ( "labels", Encode.list Encode.string data.labels )
        , ( "datasets", Encode.list encodeDataSet data.datasets )
        ]


encodeDataSet : DataSet -> Value
encodeDataSet dataSet =
    Encode.object
        [ ( "label", Encode.string dataSet.label )
        , ( "borderColor", Encode.string dataSet.borderColor )
        , ( "backgroundColor", Encode.string dataSet.backgroundColor )
        , ( "pointBackgroundColor", Encode.string dataSet.pointBackgroundColor )
        , ( "data", Encode.list Encode.float dataSet.data )
        , ( "stack", maybe Encode.string dataSet.stack )
        ]
