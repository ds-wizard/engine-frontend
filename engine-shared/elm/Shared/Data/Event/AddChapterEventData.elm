module Shared.Data.Event.AddChapterEventData exposing
    ( AddChapterEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias AddChapterEventData =
    { title : String
    , text : Maybe String
    , annotations : Dict String String
    }


decoder : Decoder AddChapterEventData
decoder =
    D.succeed AddChapterEventData
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "annotations" (D.dict D.string)


encode : AddChapterEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddChapterEvent" )
    , ( "title", E.string data.title )
    , ( "text", E.maybe E.string data.text )
    , ( "annotations", E.dict identity E.string data.annotations )
    ]
