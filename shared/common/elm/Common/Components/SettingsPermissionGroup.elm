module Common.Components.SettingsPermissionGroup exposing
    ( ViewProps
    , view
    )

import Common.Api.Models.RolePermission as RolePermission exposing (PermissionDescriptor, PermissionGroup, RolePermission)
import Common.Utils.Markdown as Markdown
import Gettext
import Html exposing (Html, div, h4, input, label, span, text)
import Html.Attributes exposing (checked, class, disabled, for, id, name, type_)
import Html.Events exposing (onCheck)


type alias ViewProps msg =
    { group : PermissionGroup
    , locale : Gettext.Locale
    , selectedPermissions : List RolePermission
    , removePermissionMsg : RolePermission -> msg
    , addPermissionMsg : RolePermission -> msg
    , disabled : Bool
    }


view : ViewProps msg -> Html msg
view props =
    div [ class "py-4" ]
        [ h4 [ class "mb-0" ] [ text (props.group.label props.locale) ]
        , div [ class "pt-2" ]
            (List.map (viewPermission props) props.group.permissions)
        ]


viewPermission : ViewProps msg -> PermissionDescriptor -> Html msg
viewPermission props descriptor =
    let
        fieldName =
            "permission-" ++ RolePermission.toString descriptor.permission

        isChecked =
            List.member descriptor.permission props.selectedPermissions

        onCheckEvent _ =
            if isChecked then
                props.removePermissionMsg descriptor.permission

            else
                props.addPermissionMsg descriptor.permission

        labelText =
            descriptor.label props.locale
    in
    div [ class "form-group border-top py-2 my-0" ]
        [ div [ class "form-check mb-2" ]
            [ label
                [ class "form-check-label form-check-toggle"
                , for fieldName
                ]
                [ input
                    [ type_ "checkbox"
                    , class "form-check-input"
                    , name fieldName
                    , id fieldName
                    , checked isChecked
                    , onCheck onCheckEvent
                    , disabled props.disabled
                    ]
                    []
                , span [] [ text labelText ]
                ]
            ]
        , Markdown.toHtml [ class "Markdown--compact text-muted" ] (descriptor.description props.locale)
        ]
