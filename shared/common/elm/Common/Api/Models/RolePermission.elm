module Common.Api.Models.RolePermission exposing
    ( PermissionDescriptor
    , PermissionGroup
    , RolePermission
    , addImpliedPermissions
    , decoder
    , encode
    , fromString
    , removeImpliedPermissions
    , toString
    )

import Gettext
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import List.Extra as List
import Maybe.Extra as Maybe


type RolePermission
    = RolePermission String


fromString : String -> RolePermission
fromString =
    RolePermission


toString : RolePermission -> String
toString (RolePermission str) =
    str


decoder : Decoder RolePermission
decoder =
    D.string
        |> D.map RolePermission


encode : RolePermission -> E.Value
encode =
    E.string << toString


type alias PermissionGroup =
    { label : Gettext.Locale -> String
    , permissions : List PermissionDescriptor
    }


type alias PermissionDescriptor =
    { permission : RolePermission
    , label : Gettext.Locale -> String
    , description : Gettext.Locale -> String
    , impliedPermissions : List RolePermission
    }


addImpliedPermissions : RolePermission -> List PermissionDescriptor -> List RolePermission
addImpliedPermissions permission descriptors =
    let
        impliedPermissions =
            List.find ((==) permission << .permission) descriptors
                |> Maybe.unwrap [] .impliedPermissions
    in
    permission :: impliedPermissions


removeImpliedPermissions : RolePermission -> List PermissionDescriptor -> List RolePermission
removeImpliedPermissions permission descriptors =
    let
        impliedPermissions =
            List.filter (List.member permission << .impliedPermissions) descriptors
                |> List.map .permission
                |> (::) permission
    in
    permission :: impliedPermissions
