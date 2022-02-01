module Shared.Data.Event.AddChapterEventData exposing
    ( AddChapterEventData
    , decoder
    , encode
    , init
    , toChapter
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)


type alias AddChapterEventData =
    { title : String
    , text : Maybe String
    , annotations : List Annotation
    }


decoder : Decoder AddChapterEventData
decoder =
    D.succeed AddChapterEventData
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddChapterEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddChapterEvent" )
    , ( "title", E.string data.title )
    , ( "text", E.maybe E.string data.text )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]


init : AddChapterEventData
init =
    { title = ""
    , text = Nothing
    , annotations = []
    }


toChapter : String -> AddChapterEventData -> Chapter
toChapter chapterUuid data =
    { uuid = chapterUuid
    , title = data.title
    , text = data.text
    , questionUuids = []
    , annotations = data.annotations
    }
