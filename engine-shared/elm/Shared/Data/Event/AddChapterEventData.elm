module Shared.Data.Event.AddChapterEventData exposing
    ( AddChapterEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias AddChapterEventData =
    { title : String
    , text : Maybe String
    }


decoder : Decoder AddChapterEventData
decoder =
    D.succeed AddChapterEventData
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)


encode : AddChapterEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddChapterEvent" )
    , ( "title", E.string data.title )
    , ( "text", E.maybe E.string data.text )
    ]
