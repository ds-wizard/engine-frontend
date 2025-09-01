module Shared.Components.AIAssistant.Models.Message exposing
    ( Message
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Message =
    { question : String
    , answer : String
    }


decoder : Decoder Message
decoder =
    D.succeed Message
        |> D.required "question" D.string
        |> D.required "answer" D.string
