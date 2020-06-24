module Shared.Data.KnowledgeModel.Tag exposing (Tag, decoder, new)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Tag =
    { uuid : String
    , name : String
    , description : Maybe String
    , color : String
    }


decoder : Decoder Tag
decoder =
    D.succeed Tag
        |> D.required "uuid" D.string
        |> D.required "name" D.string
        |> D.required "description" (D.nullable D.string)
        |> D.required "color" D.string


new : String -> Tag
new uuid =
    { uuid = uuid
    , name = "New Tag"
    , description = Nothing
    , color = "#3498DB"
    }
