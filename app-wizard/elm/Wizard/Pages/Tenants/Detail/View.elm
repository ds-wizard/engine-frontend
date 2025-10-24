module Wizard.Pages.Tenants.Detail.View exposing (view)

import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (faEdit, faWarning, fas)
import Common.Components.FormGroup as FormGroup
import Common.Components.Modal as Modal
import Common.Components.Page as Page
import Common.Utils.Markdown as Markdown
import Common.Utils.TimeUtils as TimeUtils
import Form
import Html exposing (Html, a, br, div, h3, span, strong, text)
import Html.Attributes exposing (class, href, target)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Maybe.Extra as Maybe
import String.Format as String
import Uuid
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Api.Models.TenantDetail exposing (TenantDetail)
import Wizard.Api.Models.TenantState as TenantState
import Wizard.Api.Models.User as User exposing (User)
import Wizard.Components.DetailPage as DetailPage
import Wizard.Components.TenantIcon as TenantIcon
import Wizard.Components.UsageTable as UsageTable
import Wizard.Components.UserIcon as UserIcon
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Tenants.Detail.Models exposing (Model)
import Wizard.Pages.Tenants.Detail.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewApp appState model) model.tenant


viewApp : AppState -> Model -> TenantDetail -> Html Msg
viewApp appState model app =
    DetailPage.container
        [ header appState app
        , content appState app
        , sidePanel appState app
        , viewEditModal appState model
        , viewEditLimitsModal appState model
        ]


header : AppState -> TenantDetail -> Html Msg
header appState tenantDetail =
    let
        actions =
            if Admin.isEnabled appState.config.admin then
                []

            else
                let
                    editAction =
                        a [ onClick EditModalOpen, dataCy "tenant-detail_edit" ]
                            [ faEdit
                            , text "Edit"
                            ]

                    editLimitsAction =
                        a [ onClick EditLimitsModalOpen, dataCy "tenant-detail_edit-limits" ]
                            [ fas "fa fa-sliders"
                            , text "Edit limits"
                            ]
                in
                [ editAction, editLimitsAction ]

        title =
            span [ class "top-header-title-with-icon" ]
                [ TenantIcon.view tenantDetail
                , text tenantDetail.name
                ]
    in
    DetailPage.header title actions


content : AppState -> TenantDetail -> Html Msg
content appState tenantDetail =
    let
        editWarning =
            Html.viewIf (Admin.isEnabled appState.config.admin) <|
                div [ class "alert alert-danger d-flex align-items-center" ]
                    [ faWarning
                    , Markdown.toHtml [] (String.format "Do not edit the tenant here. Go to [Admin Center](%s)." [ "/admin/tenants/" ++ Uuid.toString tenantDetail.uuid ])
                    ]
    in
    DetailPage.content
        [ editWarning
        , div [ DetailPage.contentInnerClass ]
            [ h3 [] [ text "Usage" ]
            , UsageTable.view appState True tenantDetail.usage
            ]
        ]


sidePanel : AppState -> TenantDetail -> Html Msg
sidePanel appState tenantDetail =
    let
        sections =
            [ sidePanelInfo appState tenantDetail
            , sidePanelUrls tenantDetail
            , sidePanelAdmins tenantDetail
            ]
    in
    DetailPage.sidePanel
        [ DetailPage.sidePanelList 12 12 <| List.filterMap identity sections ]


sidePanelInfo : AppState -> TenantDetail -> Maybe ( String, String, Html msg )
sidePanelInfo appState tenantDetail =
    let
        enabledBadge =
            if tenantDetail.enabled then
                Badge.success [] [ text "Enabled" ]

            else
                Badge.danger [] [ text "Disabled" ]

        infoList =
            [ ( "Tenant ID", "tenant-id", text tenantDetail.tenantId )
            , ( "Enabled", "enabled", enabledBadge )
            , ( "State", "state", text (TenantState.toReadableString tenantDetail.state) )
            , ( "Created at", "created-at", text <| TimeUtils.toReadableDateTime appState.timeZone tenantDetail.createdAt )
            , ( "Updated at", "updated-at", text <| TimeUtils.toReadableDateTime appState.timeZone tenantDetail.updatedAt )
            ]
    in
    Just ( "Info", "info", DetailPage.sidePanelList 4 8 infoList )


sidePanelUrls : TenantDetail -> Maybe ( String, String, Html msg )
sidePanelUrls tenantDetail =
    let
        urlsList =
            [ ( "Client URL", "client-url", a [ href tenantDetail.clientUrl, target "_blank" ] [ text tenantDetail.clientUrl ] )
            , ( "Server URL", "server-url", a [ href tenantDetail.serverUrl, target "_blank" ] [ text tenantDetail.serverUrl ] )
            ]
    in
    Just ( "URLs", "urls", DetailPage.sidePanelList 4 8 urlsList )


sidePanelAdmins : TenantDetail -> Maybe ( String, String, Html msg )
sidePanelAdmins tenantDetail =
    let
        users =
            tenantDetail.users
                --|> List.filter .active
                |> List.sortWith User.compare
                |> List.map viewUser
    in
    Just ( "Admins", "admins", div [] users )


viewUser : User -> Html msg
viewUser user =
    DetailPage.sidePanelHtmlItemWithIcon
        [ strong []
            [ text (User.fullName user)
            , Html.viewIf (not user.active) <| Badge.danger [ class "ms-1" ] [ text "inactive" ]
            ]
        , br [] []
        , a
            [ href ("mailto:" ++ user.email)
            , class "d-block"
            ]
            [ text user.email
            ]
        ]
        (UserIcon.viewUser user)


viewEditModal : AppState -> Model -> Html Msg
viewEditModal appState model =
    let
        modalContent =
            case model.editForm of
                Just form ->
                    [ Html.map EditModalFormMsg <| FormGroup.input appState.locale form "tenantId" "Tenant ID"
                    , Html.map EditModalFormMsg <| FormGroup.input appState.locale form "name" "Name"
                    ]

                Nothing ->
                    []

        config =
            Modal.confirmConfig "Edit app"
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible (Maybe.isJust model.editForm)
                |> Modal.confirmConfigActionResult model.savingTenant
                |> Modal.confirmConfigAction "Save" (EditModalFormMsg Form.Submit)
                |> Modal.confirmConfigCancelMsg EditModalClose
                |> Modal.confirmConfigDataCy "tenant-edit"
    in
    Modal.confirm appState config


viewEditLimitsModal : AppState -> Model -> Html Msg
viewEditLimitsModal appState model =
    let
        modalContent =
            case model.limitsForm of
                Just form ->
                    [ Html.map EditLimitsModalFormMsg <| FormGroup.input appState.locale form "users" "Users"
                    , Html.map EditLimitsModalFormMsg <| FormGroup.input appState.locale form "activeUsers" "Active Users"
                    , Html.map EditLimitsModalFormMsg <| FormGroup.input appState.locale form "knowledgeModelEditors" "Knowledge Model Editors"
                    , Html.map EditLimitsModalFormMsg <| FormGroup.input appState.locale form "knowledgeModels" "Knowledge Models"
                    , Html.map EditLimitsModalFormMsg <| FormGroup.input appState.locale form "documentTemplateDrafts" "Document Template Editors"
                    , Html.map EditLimitsModalFormMsg <| FormGroup.input appState.locale form "documentTemplates" "Document Templates"
                    , Html.map EditLimitsModalFormMsg <| FormGroup.input appState.locale form "questionnaires" "Projects"
                    , Html.map EditLimitsModalFormMsg <| FormGroup.input appState.locale form "documents" "Documents"
                    , Html.map EditLimitsModalFormMsg <| FormGroup.input appState.locale form "locales" "Locales"
                    , Html.map EditLimitsModalFormMsg <| FormGroup.fileSize appState.locale form "storage" "Storage"
                    ]

                Nothing ->
                    []

        config =
            Modal.confirmConfig "Edit limits"
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible (Maybe.isJust model.limitsForm)
                |> Modal.confirmConfigActionResult model.savingTenant
                |> Modal.confirmConfigAction "Save" (EditLimitsModalFormMsg Form.Submit)
                |> Modal.confirmConfigCancelMsg EditLimitsModalClose
                |> Modal.confirmConfigDataCy "tenant-edit-limits"
    in
    Modal.confirm appState config
