module KMEditor.Common.Events.AddChapterEventData exposing
    ( AddChapterEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias AddChapterEventData =
    { title : String
    , text : String
    }


decoder : Decoder AddChapterEventData
decoder =
    D.succeed AddChapterEventData
        |> D.required "title" D.string
        |> D.required "text" D.string


encode : AddChapterEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddChapterEvent" )
    , ( "title", E.string data.title )
    , ( "text", E.string data.text )
    ]
