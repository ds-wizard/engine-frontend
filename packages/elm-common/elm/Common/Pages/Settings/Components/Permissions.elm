module Common.Pages.Settings.Components.Permissions exposing (add, remove, view)

{-| A role's permissions: every group as a fieldset of switches, and what
switching one on or off does to the others it implies or depends on.

Which permissions there are to give is the mount's (`groups`): a role of the
organization and a role of a workspace are not offered the same ones. `note` is
something the mount has to say about them, above the groups, and `reach` a
badge on the switches of the permissions that hold beyond the role's own place.

A permission a switch implies but no switch offers goes with that switch, on
and off: one switch can stand for several permissions.

-}

import Common.Api.Models.RolePermission as RolePermission exposing (PermissionGroup, RolePermission)
import Common.Components.RolePermissions as RolePermissions
import Common.Data.AppState exposing (AppState)
import Html exposing (Html)
import List.Extra as List
import Ui.Alert as Alert
import Ui.Layout as Layout


{-| Adding a permission adds what it implies too, and what that implies.
-}
add : List PermissionGroup -> RolePermission -> List RolePermission -> List RolePermission
add groups permission permissions =
    let
        descriptors =
            List.concatMap .permissions groups

        close added =
            let
                next =
                    List.unique (List.concatMap (\p -> RolePermission.addImpliedPermissions p descriptors) added)
            in
            if List.length next == List.length added then
                added

            else
                close next
    in
    (permissions ++ close [ permission ])
        |> List.unique
        |> List.sortBy RolePermission.toString


{-| Removing a permission removes what depends on it too, and what those imply
that no switch offers.
-}
remove : List PermissionGroup -> RolePermission -> List RolePermission -> List RolePermission
remove groups permission permissions =
    let
        descriptors =
            List.concatMap .permissions groups

        offered =
            List.map .permission descriptors

        removed =
            RolePermission.removeImpliedPermissions permission descriptors

        unoffered =
            List.concatMap (\p -> RolePermission.addImpliedPermissions p descriptors) removed
                |> List.filter (\p -> List.notMember p offered)
    in
    List.filter (\p -> List.notMember p (removed ++ unoffered)) permissions


{-| `onChange` is given the permissions as they are after a switch is turned,
with what it implies or what depended on it already resolved.
-}
view :
    AppState
    ->
        { groups : List PermissionGroup
        , note : Maybe String
        , reach : Maybe String
        , selected : List RolePermission
        , onChange : List RolePermission -> msg
        , disabled : Bool
        }
    -> Html msg
view appState config =
    let
        note =
            case config.note of
                Just text ->
                    [ Alert.toHtml (Alert.info text) ]

                Nothing ->
                    []

        groups =
            List.map
                (\group ->
                    RolePermissions.view appState
                        { group = group
                        , reach = config.reach
                        , selected = config.selected
                        , onAdd = \permission -> config.onChange (add config.groups permission config.selected)
                        , onRemove = \permission -> config.onChange (remove config.groups permission config.selected)
                        , disabled = config.disabled
                        }
                )
                config.groups
    in
    Layout.stack (note ++ groups)
