module Common.Api.Models.BuildInfo.BuildInfoComponent exposing
    ( BuildInfoComponent
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias BuildInfoComponent =
    { name : String
    , version : String
    , builtAt : String
    }


decoder : Decoder BuildInfoComponent
decoder =
    D.succeed BuildInfoComponent
        |> D.required "name" D.string
        |> D.required "version" D.string
        |> D.required "builtAt" D.string
