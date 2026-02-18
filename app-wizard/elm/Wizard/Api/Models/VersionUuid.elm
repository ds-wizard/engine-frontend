module Wizard.Api.Models.VersionUuid exposing
    ( VersionUuid
    , compare
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias VersionUuid =
    { uuid : Uuid
    , version : Version
    }


decoder : Decoder VersionUuid
decoder =
    D.succeed VersionUuid
        |> D.required "uuid" Uuid.decoder
        |> D.required "version" Version.decoder


compare : VersionUuid -> VersionUuid -> Order
compare a b =
    Version.compare a.version b.version
