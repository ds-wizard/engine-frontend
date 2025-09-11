module Common.Api.Models.BuildInfo exposing (BuildInfo, client, decoder)

import Common.Api.Models.BuildInfo.BuildInfoComponent as BuildInfoComponent exposing (BuildInfoComponent)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias BuildInfo =
    { version : String
    , builtAt : String
    , components : List BuildInfoComponent
    }


client : BuildInfo
client =
    { version = "{version}"
    , builtAt = "{builtAt}"
    , components = []
    }


decoder : Decoder BuildInfo
decoder =
    D.succeed BuildInfo
        |> D.required "version" D.string
        |> D.required "builtAt" D.string
        |> D.required "components" (D.list BuildInfoComponent.decoder)
