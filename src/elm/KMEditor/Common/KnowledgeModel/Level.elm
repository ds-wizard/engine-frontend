module KMEditor.Common.KnowledgeModel.Level exposing (Level, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Level =
    { level : Int
    , title : String
    }


decoder : Decoder Level
decoder =
    D.succeed Level
        |> D.required "level" D.int
        |> D.required "title" D.string
