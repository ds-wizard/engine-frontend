module Shared.Data.BuildInfo exposing (BuildInfo, client, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias BuildInfo =
    { version : String
    , builtAt : String
    }


client : BuildInfo
client =
    { version = "{version}"
    , builtAt = "{builtAt}"
    }


decoder : Decoder BuildInfo
decoder =
    D.succeed BuildInfo
        |> D.required "version" D.string
        |> D.required "builtAt" D.string
