module Shared.Data.KnowledgeModel.Chapter exposing
    ( Chapter
    , decoder
    , new
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Chapter =
    { uuid : String
    , title : String
    , text : Maybe String
    , questionUuids : List String
    , annotations : Dict String String
    }


new : String -> Chapter
new uuid =
    { uuid = uuid
    , title = "New chapter"
    , text = Just "Chapter text"
    , questionUuids = []
    , annotations = Dict.empty
    }


decoder : Decoder Chapter
decoder =
    D.succeed Chapter
        |> D.required "uuid" D.string
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "questionUuids" (D.list D.string)
        |> D.required "annotations" (D.dict D.string)
