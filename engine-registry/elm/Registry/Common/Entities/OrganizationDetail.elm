module Registry.Common.Entities.OrganizationDetail exposing
    ( OrganizationDetail
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias OrganizationDetail =
    { name : String
    , description : String
    , email : String
    , token : String
    }


decoder : Decoder OrganizationDetail
decoder =
    D.succeed OrganizationDetail
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "email" D.string
        |> D.required "token" D.string
