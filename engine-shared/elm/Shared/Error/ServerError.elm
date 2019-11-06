module Shared.Error.ServerError exposing
    ( ServerError
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias ServerError =
    { message : String
    , fieldErrors : List ( String, String )
    }


decoder : Decoder ServerError
decoder =
    D.succeed ServerError
        |> D.optional "message" D.string ""
        |> D.optional "fieldErrors" (D.list fieldErrorDecoder) []


fieldErrorDecoder : Decoder ( String, String )
fieldErrorDecoder =
    D.map2 (\a b -> ( a, b )) (D.index 0 D.string) (D.index 1 D.string)
