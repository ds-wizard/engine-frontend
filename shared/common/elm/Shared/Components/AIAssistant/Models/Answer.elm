module Shared.Components.AIAssistant.Models.Answer exposing
    ( Answer
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Answer =
    { answer : String
    }


decoder : Decoder Answer
decoder =
    D.succeed Answer
        |> D.required "answer" D.string
