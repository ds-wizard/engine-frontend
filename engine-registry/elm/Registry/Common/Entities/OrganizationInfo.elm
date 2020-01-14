module Registry.Common.Entities.OrganizationInfo exposing
    ( OrganizationInfo
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias OrganizationInfo =
    { organizationId : String
    , name : String
    , logo : Maybe String
    }


decoder : Decoder OrganizationInfo
decoder =
    D.succeed OrganizationInfo
        |> D.required "organizationId" D.string
        |> D.required "name" D.string
        |> D.required "logo" (D.maybe D.string)
