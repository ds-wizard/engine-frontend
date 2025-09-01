module Registry.Api.Models.Organization exposing
    ( Organization
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Organization =
    { organizationId : String
    , name : String
    , description : String
    , email : String
    , token : String
    }


decoder : Decoder Organization
decoder =
    D.succeed Organization
        |> D.required "organizationId" D.string
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "email" D.string
        |> D.required "token" D.string
