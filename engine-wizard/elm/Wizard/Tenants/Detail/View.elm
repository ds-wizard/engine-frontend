module Wizard.Tenants.Detail.View exposing (view)

import Form
import Gettext exposing (gettext)
import Html exposing (Html, a, div, h3, span, text)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Maybe.Extra as Maybe
import Registry.Components.FontAwesome exposing (fas)
import Shared.Common.TimeUtils as TimeUtils
import Shared.Components.Badge as Badge
import Shared.Data.BootstrapConfig.Admin as Admin
import Shared.Data.TenantDetail exposing (TenantDetail)
import Shared.Data.User as User exposing (User)
import Shared.Html exposing (faSet)
import Shared.Markdown as Markdown
import Shared.Utils exposing (listFilterJust)
import String.Format as String
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.DetailPage as DetailPage
import Wizard.Common.Components.UsageTable as UsageTable
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Common.View.TenantIcon as TenantIcon
import Wizard.Common.View.UserIcon as UserIcon
import Wizard.Tenants.Detail.Models exposing (Model)
import Wizard.Tenants.Detail.Msgs exposing (Msg(..))


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
        editAction =
            a [ onClick EditModalOpen, dataCy "tenant-detail_edit" ]
                [ faSet "_global.edit" appState
                , text (gettext "Edit" appState.locale)
                ]

        editLimitsAction =
            a [ onClick EditLimitsModalOpen, dataCy "tenant-detail_edit-limits" ]
                [ fas "fa fa-sliders"
                , text (gettext "Edit limits" appState.locale)
                ]

        title =
            span [ class "top-header-title-with-icon" ]
                [ TenantIcon.view tenantDetail
                , text tenantDetail.name
                ]
    in
    DetailPage.header title [ editAction, editLimitsAction ]


content : AppState -> TenantDetail -> Html Msg
content appState tenantDetail =
    let
        editWarning =
            Html.viewIf (Admin.isEnabled appState.config.admin) <|
                div [ class "alert alert-danger d-flex align-items-center" ]
                    [ faSet "_global.warning" appState
                    , Markdown.toHtml [] (String.format "Do not edit the tenant here. Go to [Admin Center](%s)." [ "/admin/tenants/" ++ Uuid.toString tenantDetail.uuid ])
                    ]
    in
    DetailPage.content
        [ editWarning
        , div [ DetailPage.contentInnerClass ]
            [ h3 [] [ text (gettext "Usage" appState.locale) ]
            , UsageTable.view appState False tenantDetail.usage
            ]
        ]


sidePanel : AppState -> TenantDetail -> Html Msg
sidePanel appState tenantDetail =
    let
        sections =
            [ sidePanelInfo appState tenantDetail
            , sidePanelUrls appState tenantDetail
            , sidePanelAdmins appState tenantDetail
            ]
    in
    DetailPage.sidePanel
        [ DetailPage.sidePanelList 12 12 <| listFilterJust sections ]


sidePanelInfo : AppState -> TenantDetail -> Maybe ( String, String, Html msg )
sidePanelInfo appState tenantDetail =
    let
        enabledBadge =
            if tenantDetail.enabled then
                Badge.success [] [ text (gettext "Enabled" appState.locale) ]

            else
                Badge.danger [] [ text (gettext "Disabled" appState.locale) ]

        infoList =
            [ ( gettext "Tenant ID" appState.locale, "tenant-id", text tenantDetail.tenantId )
            , ( gettext "Enabled" appState.locale, "enabled", enabledBadge )
            , ( gettext "Created at" appState.locale, "created-at", text <| TimeUtils.toReadableDateTime appState.timeZone tenantDetail.createdAt )
            , ( gettext "Updated at" appState.locale, "updated-at", text <| TimeUtils.toReadableDateTime appState.timeZone tenantDetail.updatedAt )
            ]
    in
    Just ( gettext "Info" appState.locale, "info", DetailPage.sidePanelList 4 8 infoList )


sidePanelUrls : AppState -> TenantDetail -> Maybe ( String, String, Html msg )
sidePanelUrls appState tenantDetail =
    let
        urlsList =
            [ ( gettext "Client URL" appState.locale, "client-url", a [ href tenantDetail.clientUrl, target "_blank" ] [ text tenantDetail.clientUrl ] )
            , ( gettext "Server URL" appState.locale, "server-url", a [ href tenantDetail.serverUrl, target "_blank" ] [ text tenantDetail.serverUrl ] )
            ]
    in
    Just ( gettext "URLs" appState.locale, "urls", DetailPage.sidePanelList 4 8 urlsList )


sidePanelAdmins : AppState -> TenantDetail -> Maybe ( String, String, Html msg )
sidePanelAdmins appState tenantDetail =
    let
        users =
            tenantDetail.users
                |> List.filter .active
                |> List.sortWith User.compare
                |> List.map viewUser
    in
    Just ( gettext "Admins" appState.locale, "admins", div [] users )


viewUser : User -> Html msg
viewUser user =
    DetailPage.sidePanelItemWithIcon (User.fullName user)
        (a [ href ("mailto:" ++ user.email) ] [ text user.email ])
        (UserIcon.viewUser user)


viewEditModal : AppState -> Model -> Html Msg
viewEditModal appState model =
    let
        modalContent =
            case model.editForm of
                Just form ->
                    [ Html.map EditModalFormMsg <| FormGroup.input appState form "tenantId" <| gettext "Tenant ID" appState.locale
                    , Html.map EditModalFormMsg <| FormGroup.input appState form "name" <| gettext "Name" appState.locale
                    ]

                Nothing ->
                    []

        config =
            { modalTitle = gettext "Edit app" appState.locale
            , modalContent = modalContent
            , visible = Maybe.isJust model.editForm
            , actionResult = model.savingTenant
            , actionName = gettext "Save" appState.locale
            , actionMsg = EditModalFormMsg Form.Submit
            , cancelMsg = Just EditModalClose
            , dangerous = False
            , dataCy = "tenant-edit"
            }
    in
    Modal.confirm appState config


viewEditLimitsModal : AppState -> Model -> Html Msg
viewEditLimitsModal appState model =
    let
        modalContent =
            case model.limitsForm of
                Just form ->
                    [ Html.map EditLimitsModalFormMsg <| FormGroup.input appState form "users" <| gettext "Users" appState.locale
                    , Html.map EditLimitsModalFormMsg <| FormGroup.input appState form "activeUsers" <| gettext "Active Users" appState.locale
                    , Html.map EditLimitsModalFormMsg <| FormGroup.input appState form "branches" <| gettext "Knowledge Model Editors" appState.locale
                    , Html.map EditLimitsModalFormMsg <| FormGroup.input appState form "knowledgeModels" <| gettext "Knowledge Models" appState.locale
                    , Html.map EditLimitsModalFormMsg <| FormGroup.input appState form "documentTemplateDrafts" <| gettext "Document Template Editors" appState.locale
                    , Html.map EditLimitsModalFormMsg <| FormGroup.input appState form "documentTemplates" <| gettext "Document Templates" appState.locale
                    , Html.map EditLimitsModalFormMsg <| FormGroup.input appState form "questionnaires" <| gettext "Projects" appState.locale
                    , Html.map EditLimitsModalFormMsg <| FormGroup.input appState form "documents" <| gettext "Documents" appState.locale
                    , Html.map EditLimitsModalFormMsg <| FormGroup.input appState form "locales" <| gettext "Locales" appState.locale
                    , Html.map EditLimitsModalFormMsg <| FormGroup.fileSize appState form "storage" <| gettext "Storage" appState.locale
                    ]

                Nothing ->
                    []

        config =
            { modalTitle = gettext "Edit limits" appState.locale
            , modalContent = modalContent
            , visible = Maybe.isJust model.limitsForm
            , actionResult = model.savingTenant
            , actionName = gettext "Save" appState.locale
            , actionMsg = EditLimitsModalFormMsg Form.Submit
            , cancelMsg = Just EditLimitsModalClose
            , dangerous = False
            , dataCy = "tenant-edit-limits"
            }
    in
    Modal.confirm appState config
