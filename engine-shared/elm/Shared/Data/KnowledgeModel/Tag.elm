module Shared.Data.KnowledgeModel.Tag exposing (Tag, decoder)

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
