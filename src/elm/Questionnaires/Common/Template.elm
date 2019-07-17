module Questionnaires.Common.Template exposing
    ( Template
    , decoder
    , listDecoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Template =
    { uuid : String
    , name : String
    }


decoder : Decoder Template
decoder =
    D.succeed Template
        |> D.required "uuid" D.string
        |> D.required "name" D.string


listDecoder : Decoder (List Template)
listDecoder =
    D.list decoder
