module Common.Api.Models.UuidResponse exposing (UuidResponse, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias UuidResponse =
    { uuid : Uuid
    }


decoder : Decoder UuidResponse
decoder =
    D.succeed UuidResponse
        |> D.required "uuid" Uuid.decoder
