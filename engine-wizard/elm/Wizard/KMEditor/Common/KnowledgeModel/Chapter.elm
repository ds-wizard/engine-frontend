module Wizard.KMEditor.Common.KnowledgeModel.Chapter exposing
    ( Chapter
    , decoder
    , new
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Chapter =
    { uuid : String
    , title : String
    , text : Maybe String
    , questionUuids : List String
    }


decoder : Decoder Chapter
decoder =
    D.succeed Chapter
        |> D.required "uuid" D.string
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "questionUuids" (D.list D.string)


new : String -> Chapter
new uuid =
    { uuid = uuid
    , title = "New chapter"
    , text = Just "Chapter text"
    , questionUuids = []
    }
