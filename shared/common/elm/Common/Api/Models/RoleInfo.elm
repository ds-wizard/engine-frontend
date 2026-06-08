module Common.Api.Models.RoleInfo exposing (RoleInfo, decoder)

import Common.Api.Models.RolePermission as RolePermission exposing (RolePermission)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias RoleInfo =
    { uuid : Uuid
    , name : String
    , permissions : List RolePermission
    }


decoder : Decoder RoleInfo
decoder =
    D.succeed RoleInfo
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "permissions" (D.list RolePermission.decoder)
