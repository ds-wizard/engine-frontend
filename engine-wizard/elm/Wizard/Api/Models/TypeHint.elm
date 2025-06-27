module Wizard.Api.Models.TypeHint exposing
    ( TypeHint
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias TypeHint =
    { id : Maybe String
    , name : String
    }


decoder : Decoder TypeHint
decoder =
    D.succeed TypeHint
        |> D.required "id" (D.maybe D.string)
        |> D.required "name" D.string
