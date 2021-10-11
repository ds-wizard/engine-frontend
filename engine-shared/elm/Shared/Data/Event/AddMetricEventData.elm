module Shared.Data.Event.AddMetricEventData exposing
    ( AddMetricEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias AddMetricEventData =
    { title : String
    , abbreviation : Maybe String
    , description : Maybe String
    , annotations : Dict String String
    }


decoder : Decoder AddMetricEventData
decoder =
    D.succeed AddMetricEventData
        |> D.required "title" D.string
        |> D.required "abbreviation" (D.maybe D.string)
        |> D.required "description" (D.maybe D.string)
        |> D.required "annotations" (D.dict D.string)


encode : AddMetricEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddMetricEvent" )
    , ( "title", E.string data.title )
    , ( "abbreviation", E.maybe E.string data.abbreviation )
    , ( "description", E.maybe E.string data.description )
    , ( "annotations", E.dict identity E.string data.annotations )
    ]
