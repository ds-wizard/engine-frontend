module Wizard.Api.Models.ProjectInfo exposing
    ( ProjectInfo
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias ProjectInfo =
    { uuid : Uuid
    , name : String
    }


decoder : Decoder ProjectInfo
decoder =
    D.succeed ProjectInfo
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
