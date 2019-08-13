module KMEditor.Common.Events.EditChapterEventData exposing
    ( EditChapterEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import KMEditor.Common.Events.EventField as EventField exposing (EventField)


type alias EditChapterEventData =
    { title : EventField String
    , text : EventField String
    , questionUuids : EventField (List String)
    }


decoder : Decoder EditChapterEventData
decoder =
    D.succeed EditChapterEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "text" (EventField.decoder D.string)
        |> D.required "questionUuids" (EventField.decoder (D.list D.string))


encode : EditChapterEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditChapterEvent" )
    , ( "title", EventField.encode E.string data.title )
    , ( "text", EventField.encode E.string data.text )
    , ( "questionUuids", EventField.encode (E.list E.string) data.questionUuids )
    ]
