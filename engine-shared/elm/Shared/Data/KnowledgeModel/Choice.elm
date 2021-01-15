module Shared.Data.KnowledgeModel.Choice exposing
    ( Choice
    , decoder
    , new
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Choice =
    { uuid : String
    , label : String
    }


new : String -> Choice
new uuid =
    { uuid = uuid
    , label = "New choice"
    }


decoder : Decoder Choice
decoder =
    D.succeed Choice
        |> D.required "uuid" D.string
        |> D.required "label" D.string
