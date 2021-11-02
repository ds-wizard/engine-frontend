module Shared.Data.KnowledgeModel.Tag exposing (Tag, decoder, new)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Tag =
    { uuid : String
    , name : String
    , description : Maybe String
    , color : String
    , annotations : Dict String String
    }


new : String -> Tag
new uuid =
    { uuid = uuid
    , name = "New Tag"
    , description = Nothing
    , color = "#3498DB"
    , annotations = Dict.empty
    }


decoder : Decoder Tag
decoder =
    D.succeed Tag
        |> D.required "uuid" D.string
        |> D.required "name" D.string
        |> D.required "description" (D.nullable D.string)
        |> D.required "color" D.string
        |> D.required "annotations" (D.dict D.string)
