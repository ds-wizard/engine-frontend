module Wizard.Pages.Settings.Roles.View exposing (view)

import Common.Api.Models.Role exposing (Role)
import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (faDelete)
import Common.Components.Modal as Modal
import Common.Components.Page as Page
import Common.Components.Tooltip exposing (tooltip, tooltipLeft)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, p, span, strong, text)
import Html.Attributes exposing (class, classList, disabled, href)
import Html.Events.Extra exposing (onClickPreventDefaultAndStopPropagation)
import Html.Extra as Html
import String.Format as String
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Roles.Models exposing (Model)
import Wizard.Pages.Settings.Roles.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routing


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewRoles appState model) model.roles


viewRoles : AppState -> Model -> List Role -> Html Msg
viewRoles appState model roles =
    div []
        [ Page.header (gettext "Roles" appState.locale)
            [ a
                [ class "btn btn-primary btn-wide"
                , href (Routing.toUrl Routes.settingsRoleCreate)
                ]
                [ text (gettext "Create" appState.locale) ]
            ]
        , div [ class "card-list" ]
            (List.map (viewRole appState) (List.sortBy .name roles))
        , viewDeleteModal appState model
        ]


viewRole : AppState -> Role -> Html Msg
viewRole appState role =
    let
        isDefaultRole =
            role.uuid == appState.config.authentication.defaultRoleUuid

        badge =
            if role.usersCount > 0 then
                Badge.info

            else
                Badge.secondary

        ( deleteEnabled, deleteTooltip ) =
            if role.isAdmin then
                ( False, gettext "Admin role cannot be deleted" appState.locale )

            else if isDefaultRole then
                ( False, gettext "Default role cannot be deleted" appState.locale )

            else if role.usersCount > 0 then
                ( False, gettext "Role cannot be deleted because it is assigned to users" appState.locale )

            else
                ( True, gettext "Delete" appState.locale )

        deleteOnClick =
            if deleteEnabled then
                [ onClickPreventDefaultAndStopPropagation (ShowHideDeleteRole (Just role)) ]

            else
                []

        defaultRoleBadge =
            if isDefaultRole then
                Badge.dark [ class "ms-2" ] [ text (gettext "Default" appState.locale) ]

            else
                Html.nothing
    in
    a
        [ class "card bg-light mb-2"
        , href (Routing.toUrl (Routes.settingsRoleDetail role.uuid))
        ]
        [ div [ class "card-body py-2 d-flex align-items-center" ]
            [ text role.name
            , badge (class "ms-2 rounded-pill" :: tooltip (gettext "Users using this role" appState.locale)) [ text (String.fromInt role.usersCount) ]
            , defaultRoleBadge
            , span (class "ms-auto" :: tooltipLeft deleteTooltip)
                [ a
                    (class "btn btn-link link-danger px-1 py-0"
                        :: classList [ ( "disabled", not deleteEnabled ) ]
                        :: disabled (not deleteEnabled)
                        :: deleteOnClick
                        ++ tooltipLeft deleteTooltip
                    )
                    [ faDelete
                    ]
                ]
            ]
        ]


viewDeleteModal : AppState -> Model -> Html Msg
viewDeleteModal appState model =
    let
        ( visible, content ) =
            case model.roleToBeDeleted of
                Just role ->
                    ( True
                    , [ p []
                            (String.formatHtml
                                (gettext "Are you sure you want to permanently delete %s?" appState.locale)
                                [ strong [] [ text role.name ] ]
                            )
                      ]
                    )

                Nothing ->
                    ( False, [] )

        cfg =
            Modal.confirmConfig (gettext "Delete OpenID" appState.locale)
                |> Modal.confirmConfigContent content
                |> Modal.confirmConfigVisible visible
                |> Modal.confirmConfigActionResult model.deletingRole
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) DeleteRole
                |> Modal.confirmConfigCancelMsg (ShowHideDeleteRole Nothing)
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "openid-delete"
    in
    Modal.confirm appState cfg
