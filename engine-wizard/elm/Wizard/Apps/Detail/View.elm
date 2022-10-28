module Wizard.Apps.Detail.View exposing (view)

import Form
import Gettext exposing (gettext)
import Html exposing (Html, a, div, h3, hr, span, text)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe
import Shared.Common.TimeUtils as TimeUtils
import Shared.Components.Badge as Badge
import Shared.Data.AppDetail exposing (AppDetail)
import Shared.Data.User as User exposing (User)
import Shared.Html exposing (faSet)
import Shared.Markdown as Markdown
import Shared.Utils exposing (flip, listFilterJust)
import String.Format as String
import Wizard.Apps.Detail.Models exposing (Model)
import Wizard.Apps.Detail.Msgs exposing (Msg(..))
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.DetailPage as DetailPage
import Wizard.Common.Components.PlansList as PlansList
import Wizard.Common.Components.UsageTable as UsageTable
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.AppIcon as AppIcon
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Common.View.UserIcon as UserIcon


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewApp appState model) model.app


viewApp : AppState -> Model -> AppDetail -> Html Msg
viewApp appState model app =
    DetailPage.container
        [ header appState app
        , content appState app
        , sidePanel appState app
        , viewEditModal appState model
        , viewAddPlanModal appState model
        , viewEditPlanModal appState model
        , viewDeletePlanModal appState model
        ]


header : AppState -> AppDetail -> Html Msg
header appState app =
    let
        editAction =
            a [ onClick EditModalOpen, dataCy "app-detail_edit" ]
                [ faSet "_global.edit" appState
                , text (gettext "Edit" appState.locale)
                ]

        title =
            span [ class "top-header-title-with-icon" ]
                [ AppIcon.view app
                , text app.name
                ]
    in
    DetailPage.header title [ editAction ]


content : AppState -> AppDetail -> Html Msg
content appState appDetail =
    let
        planActions plan =
            [ a [ onClick (EditPlanModalOpen plan), dataCy "app-detail_plan_edit" ] [ faSet "_global.edit" appState ]
            , a [ onClick (DeletePlanModalOpen plan), class "text-danger ms-3", dataCy "app-detail_plan_delete" ] [ faSet "_global.delete" appState ]
            ]
    in
    DetailPage.content
        [ div [ DetailPage.contentInnerClass ]
            [ h3 [] [ text (gettext "Usage" appState.locale) ]
            , UsageTable.view appState appDetail.usage
            , hr [ class "my-5" ] []
            , h3 [] [ text (gettext "Plans" appState.locale) ]
            , PlansList.view appState { actions = Just planActions } appDetail.plans
            , a [ class "with-icon", onClick AddPlanModalOpen, dataCy "app-detail_add-plan" ]
                [ faSet "_global.add" appState, text (gettext "Add plan" appState.locale) ]
            ]
        ]


sidePanel : AppState -> AppDetail -> Html Msg
sidePanel appState appDetail =
    let
        sections =
            [ sidePanelInfo appState appDetail
            , sidePanelUrls appState appDetail
            , sidePanelAdmins appState appDetail
            ]
    in
    DetailPage.sidePanel
        [ DetailPage.sidePanelList 12 12 <| listFilterJust sections ]


sidePanelInfo : AppState -> AppDetail -> Maybe ( String, String, Html msg )
sidePanelInfo appState appDetail =
    let
        enabledBadge =
            if appDetail.enabled then
                Badge.success [] [ text (gettext "Enabled" appState.locale) ]

            else
                Badge.danger [] [ text (gettext "Disabled" appState.locale) ]

        infoList =
            [ ( gettext "App ID" appState.locale, "app-id", text appDetail.appId )
            , ( gettext "Enabled" appState.locale, "enabled", enabledBadge )
            , ( gettext "Created at" appState.locale, "created-at", text <| TimeUtils.toReadableDateTime appState.timeZone appDetail.createdAt )
            , ( gettext "Updated at" appState.locale, "updated-at", text <| TimeUtils.toReadableDateTime appState.timeZone appDetail.updatedAt )
            ]
    in
    Just ( gettext "Info" appState.locale, "info", DetailPage.sidePanelList 4 8 infoList )


sidePanelUrls : AppState -> AppDetail -> Maybe ( String, String, Html msg )
sidePanelUrls appState appDetail =
    let
        urlsList =
            [ ( gettext "Client URL" appState.locale, "client-url", a [ href appDetail.clientUrl, target "_blank" ] [ text appDetail.clientUrl ] )
            , ( gettext "Server URL" appState.locale, "server-url", a [ href appDetail.serverUrl, target "_blank" ] [ text appDetail.serverUrl ] )
            ]
    in
    Just ( gettext "URLs" appState.locale, "urls", DetailPage.sidePanelList 4 8 urlsList )


sidePanelAdmins : AppState -> AppDetail -> Maybe ( String, String, Html msg )
sidePanelAdmins appState appDetail =
    let
        users =
            appDetail.users
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
                    [ Html.map EditModalFormMsg <| FormGroup.input appState form "appId" <| gettext "App ID" appState.locale
                    , Html.map EditModalFormMsg <| FormGroup.input appState form "name" <| gettext "Name" appState.locale
                    ]

                Nothing ->
                    []

        config =
            { modalTitle = gettext "Edit app" appState.locale
            , modalContent = modalContent
            , visible = Maybe.isJust model.editForm
            , actionResult = model.savingApp
            , actionName = gettext "Save" appState.locale
            , actionMsg = EditModalFormMsg Form.Submit
            , cancelMsg = Just EditModalClose
            , dangerous = False
            , dataCy = "app-edit"
            }
    in
    Modal.confirm appState config


viewAddPlanModal : AppState -> Model -> Html Msg
viewAddPlanModal appState model =
    let
        modalContent =
            case model.addPlanForm of
                Just form ->
                    [ Html.map AddPlanModalFormMsg <| FormGroup.input appState form "name" <| gettext "Name" appState.locale
                    , Html.map AddPlanModalFormMsg <| FormGroup.input appState form "users" <| gettext "Users" appState.locale
                    , Html.map AddPlanModalFormMsg <| FormGroup.simpleDate appState form "sinceYear" "sinceMonth" "sinceDay" <| gettext "From" appState.locale
                    , Html.map AddPlanModalFormMsg <| FormGroup.simpleDate appState form "untilYear" "untilMonth" "untilDay" <| gettext "To" appState.locale
                    , Html.map AddPlanModalFormMsg <| FormGroup.toggle form "test" <| gettext "Trial" appState.locale
                    ]

                Nothing ->
                    []

        config =
            { modalTitle = gettext "Add plan" appState.locale
            , modalContent = modalContent
            , visible = Maybe.isJust model.addPlanForm
            , actionResult = model.addingPlan
            , actionName = gettext "Add" appState.locale
            , actionMsg = AddPlanModalFormMsg Form.Submit
            , cancelMsg = Just AddPlanModalClose
            , dangerous = False
            , dataCy = "app_plan-add"
            }
    in
    Modal.confirm appState config


viewEditPlanModal : AppState -> Model -> Html Msg
viewEditPlanModal appState model =
    let
        modalContent =
            case model.editPlanForm of
                Just ( _, form ) ->
                    [ Html.map EditPlanModalFormMsg <| FormGroup.input appState form "name" <| gettext "Name" appState.locale
                    , Html.map EditPlanModalFormMsg <| FormGroup.input appState form "users" <| gettext "Users" appState.locale
                    , Html.map EditPlanModalFormMsg <| FormGroup.simpleDate appState form "sinceYear" "sinceMonth" "sinceDay" <| gettext "From" appState.locale
                    , Html.map EditPlanModalFormMsg <| FormGroup.simpleDate appState form "untilYear" "untilMonth" "untilDay" <| gettext "To" appState.locale
                    , Html.map EditPlanModalFormMsg <| FormGroup.toggle form "test" <| gettext "Trial" appState.locale
                    ]

                Nothing ->
                    []

        config =
            { modalTitle = gettext "Edit plan" appState.locale
            , modalContent = modalContent
            , visible = Maybe.isJust model.editPlanForm
            , actionResult = model.editingPlan
            , actionName = gettext "Save" appState.locale
            , actionMsg = EditPlanModalFormMsg Form.Submit
            , cancelMsg = Just EditPlanModalClose
            , dangerous = False
            , dataCy = "app_plan-edit"
            }
    in
    Modal.confirm appState config


viewDeletePlanModal : AppState -> Model -> Html Msg
viewDeletePlanModal appState model =
    let
        modalContent =
            case model.deletePlan of
                Just plan ->
                    gettext "Are you sure you want to delete plan **%s**?" appState.locale
                        |> flip String.format [ plan.name ]
                        |> Markdown.toHtml []
                        |> List.singleton

                Nothing ->
                    []

        config =
            { modalTitle = gettext "Delete plan" appState.locale
            , modalContent = modalContent
            , visible = Maybe.isJust model.deletePlan
            , actionResult = model.deletingPlan
            , actionName = gettext "Delete" appState.locale
            , actionMsg = DeletePlanModalConfirm
            , cancelMsg = Just DeletePlanModalClose
            , dangerous = True
            , dataCy = "app_plan-delete"
            }
    in
    Modal.confirm appState config
