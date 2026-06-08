module Common.Api.Models.Role exposing (Role, decoder, toFormOption)

import Common.Api.Models.RolePermission as RolePermission exposing (RolePermission)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias Role =
    { uuid : Uuid
    , isAdmin : Bool
    , name : String
    , permissions : List RolePermission
    , usersCount : Int
    }


decoder : Decoder Role
decoder =
    D.succeed Role
        |> D.required "uuid" Uuid.decoder
        |> D.required "isAdmin" D.bool
        |> D.required "name" D.string
        |> D.required "permissions" (D.list RolePermission.decoder)
        |> D.required "usersCount" D.int


toFormOption : Role -> ( String, String )
toFormOption role =
    ( Uuid.toString role.uuid, role.name )
