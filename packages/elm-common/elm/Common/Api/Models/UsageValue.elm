module Common.Api.Models.UsageValue exposing
    ( UsageValue
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias UsageValue =
    { max : Int
    , current : Int
    }


decoder : Decoder UsageValue
decoder =
    D.succeed UsageValue
        |> D.required "max" D.int
        |> D.required "current" D.int
