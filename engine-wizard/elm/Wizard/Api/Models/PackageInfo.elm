module Wizard.Api.Models.PackageInfo exposing
    ( PackageInfo
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Version exposing (Version)


type alias PackageInfo =
    { id : String
    , name : String
    , version : Version
    }


decoder : Decoder PackageInfo
decoder =
    D.succeed PackageInfo
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "version" Version.decoder
