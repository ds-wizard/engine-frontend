module Wizard.Api.Models.AppKey exposing (AppKey, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)


type alias AppKey =
    { uuid : Uuid
    , name : String
    , createdAt : Time.Posix
    }


decoder : Decoder AppKey
decoder =
    D.succeed AppKey
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "createdAt" D.datetime
