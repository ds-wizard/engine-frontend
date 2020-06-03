module Shared.Data.Token exposing
    ( Token
    , decoder
    , value
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type Token
    = Token Internals


type alias Internals =
    { value : String }


value : Token -> String
value (Token token) =
    token.value


decoder : Decoder Token
decoder =
    D.succeed Internals
        |> D.required "token" D.string
        |> D.map Token
