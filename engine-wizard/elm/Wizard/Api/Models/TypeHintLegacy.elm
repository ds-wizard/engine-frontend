module Wizard.Api.Models.TypeHintLegacy exposing
    ( TypeHintLegacy
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias TypeHintLegacy =
    { id : Maybe String
    , name : String
    }


decoder : Decoder TypeHintLegacy
decoder =
    D.succeed TypeHintLegacy
        |> D.required "id" (D.maybe D.string)
        |> D.required "name" D.string
