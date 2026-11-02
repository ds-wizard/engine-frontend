module Common.Components.RolePermissions exposing (view)

{-| One group of a role's permissions: a switch for each permission, inside a
`fieldset` named by the group.

As the wizard had them — one switch per permission with what it allows under
it — rather than a list of choices. The `fieldset` names the set, so a screen
reader says "Projects, group" on the way in and every switch is heard as part of
it; each switch is named by its permission and described by its sentence
(`Ui.Field.withHint`).

The descriptions are written in markdown for the existing page, where the only
formatting in them is emphasis on the names of the permissions they require. A
hint is text, so they are flattened: the words are kept and the emphasis is
dropped.

`reach` is a badge for the switches of the permissions that can be held in more
than one place (`OrganizationAndWorkspace`): beside the name, so it is heard
with it.

Switching on a permission that implies others, or off one others depend on, is
the caller's to resolve — `onAdd` and `onRemove` receive the permission, and
`RolePermission.addImpliedPermissions` and `removeImpliedPermissions` do the rest,
as they do today.

-}

import Common.Api.Models.RolePermission as RolePermission exposing (PermissionDescriptor, PermissionGroup, RolePermission)
import Common.Data.Context exposing (WithLocale)
import Common.Utils.Markdown as Markdown
import Html exposing (Html)
import Ui.Badge as Badge
import Ui.Field as Field


view :
    WithLocale a
    ->
        { group : PermissionGroup
        , reach : Maybe String
        , selected : List RolePermission
        , onAdd : RolePermission -> msg
        , onRemove : RolePermission -> msg
        , disabled : Bool
        }
    -> Html msg
view appState config =
    let
        groupId =
            config.group.permissions
                |> List.head
                |> Maybe.map (\descriptor -> permissionId descriptor.permission)
                |> Maybe.withDefault "empty"
    in
    Field.group
        { id = "permission-group-" ++ groupId
        , legend = config.group.label appState.locale
        , fields = List.map (permissionToggle appState config) config.group.permissions
        }
        |> Field.withGroupStacked
        |> Field.groupToHtml


permissionToggle :
    WithLocale a
    -> { c | reach : Maybe String, selected : List RolePermission, onAdd : RolePermission -> msg, onRemove : RolePermission -> msg, disabled : Bool }
    -> PermissionDescriptor
    -> Html msg
permissionToggle appState config descriptor =
    let
        withReach field =
            case ( config.reach, descriptor.scope ) of
                ( Just reach, RolePermission.OrganizationAndWorkspace ) ->
                    Field.withLabelBadge (Badge.neutral reach) field

                _ ->
                    field

        onCheck checked =
            if checked then
                config.onAdd descriptor.permission

            else
                config.onRemove descriptor.permission
    in
    Field.toggle
        { id = "permission-" ++ permissionId descriptor.permission
        , label = descriptor.label appState.locale
        , checked = List.member descriptor.permission config.selected
        , onCheck = onCheck
        }
        |> Field.withHint (Markdown.toString (descriptor.description appState.locale))
        |> withReach
        |> Field.withDisabled config.disabled
        |> Field.toHtml


{-| A permission as part of an element's id: its string, without the dot.
-}
permissionId : RolePermission -> String
permissionId =
    RolePermission.toString >> String.replace "." "-"
