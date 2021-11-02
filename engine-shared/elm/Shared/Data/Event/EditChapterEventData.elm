module Shared.Data.Event.EditChapterEventData exposing
    ( EditChapterEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.Event.EventField as EventField exposing (EventField)


type alias EditChapterEventData =
    { title : EventField String
    , text : EventField (Maybe String)
    , questionUuids : EventField (List String)
    , annotations : EventField (Dict String String)
    }


decoder : Decoder EditChapterEventData
decoder =
    D.succeed EditChapterEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "text" (EventField.decoder (D.nullable D.string))
        |> D.required "questionUuids" (EventField.decoder (D.list D.string))
        |> D.required "annotations" (EventField.decoder (D.dict D.string))


encode : EditChapterEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditChapterEvent" )
    , ( "title", EventField.encode E.string data.title )
    , ( "text", EventField.encode (E.maybe E.string) data.text )
    , ( "questionUuids", EventField.encode (E.list E.string) data.questionUuids )
    , ( "annotations", EventField.encode (E.dict identity E.string) data.annotations )
    ]
