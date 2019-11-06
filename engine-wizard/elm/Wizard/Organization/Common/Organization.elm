module Wizard.Organization.Common.Organization exposing
    ( Organization
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Organization =
    { uuid : String
    , name : String
    , organizationId : String
    }


decoder : Decoder Organization
decoder =
    D.succeed Organization
        |> D.required "uuid" D.string
        |> D.required "name" D.string
        |> D.required "organizationId" D.string
