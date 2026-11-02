module Common.Api.Models.RolePermission exposing
    ( PermissionDescriptor
    , PermissionGroup
    , PermissionScope(..)
    , RolePermission
    , addImpliedPermissions
    , decoder
    , encode
    , fromString
    , inWorkspace
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


{-| Where a permission can be given.

  - `OrganizationOnly` — only on a role of the organization: it is about
    something there is one of.
  - `OrganizationAndWorkspace` — on a role of the organization, where it holds
    in every workspace, or on a role of one workspace.

-}
type PermissionScope
    = OrganizationOnly
    | OrganizationAndWorkspace


type alias PermissionDescriptor =
    { permission : RolePermission
    , scope : PermissionScope
    , label : Gettext.Locale -> String
    , description : Gettext.Locale -> String
    , impliedPermissions : List RolePermission
    }


{-| The groups as a role of a workspace has them: only what can be given there,
and no group left empty.
-}
inWorkspace : List PermissionGroup -> List PermissionGroup
inWorkspace =
    List.map (\group -> { group | permissions = List.filter ((==) OrganizationAndWorkspace << .scope) group.permissions })
        >> List.filterNot (List.isEmpty << .permissions)


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
