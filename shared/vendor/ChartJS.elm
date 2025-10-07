-- TODO: Remove this module in favor of the shared chart components


module ChartJS exposing
    ( Data
    , DataSet
    , chartData
    , encodeData
    , radarChart
    )

import Html exposing (Html)
import Html.Attributes
import Json.Encode as E exposing (Value)
import Json.Encode.Extra as E
import Round


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


radarChart : List (Html.Attribute msg) -> Html msg
radarChart attributes =
    Html.node "chart-radar" attributes []


chartData : Data -> Html.Attribute msg
chartData =
    Html.Attributes.property "chartData" << encodeData


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
