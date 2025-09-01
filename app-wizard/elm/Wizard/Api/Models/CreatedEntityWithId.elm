module Wizard.Api.Models.CreatedEntityWithId exposing (CreatedEntityWithId, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias CreatedEntityWithId =
    { id : String
    }


decoder : Decoder CreatedEntityWithId
decoder =
    D.succeed CreatedEntityWithId
        |> D.required "id" D.string
