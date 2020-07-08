module Shared.Data.TypeHint exposing
    ( TypeHint
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias TypeHint =
    { id : String
    , name : String
    }


decoder : Decoder TypeHint
decoder =
    D.succeed TypeHint
        |> D.required "id" D.string
        |> D.required "name" D.string
