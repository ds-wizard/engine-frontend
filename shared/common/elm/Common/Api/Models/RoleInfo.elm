module Common.Api.Models.RoleInfo exposing (RoleInfo, decoder, encode)

import Common.Api.Models.RolePermission as RolePermission exposing (RolePermission)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
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


encode : RoleInfo -> E.Value
encode roleInfo =
    E.object
        [ ( "uuid", Uuid.encode roleInfo.uuid )
        , ( "name", E.string roleInfo.name )
        , ( "permissions", E.list RolePermission.encode roleInfo.permissions )
        ]
