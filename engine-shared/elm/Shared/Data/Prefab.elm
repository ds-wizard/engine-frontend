module Shared.Data.Prefab exposing
    ( Prefab
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Prefab a =
    { content : a }


decoder : Decoder a -> Decoder (Prefab a)
decoder contentDecoder =
    D.succeed Prefab
        |> D.required "content" contentDecoder
