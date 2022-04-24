module Wizard.Apps.Detail.View exposing (view)

import Form
import Html exposing (Html, a, div, h3, hr, span, text)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe
import Shared.Common.TimeUtils as TimeUtils
import Shared.Data.AppDetail exposing (AppDetail)
import Shared.Data.User as User exposing (User)
import Shared.Html exposing (faSet)
import Shared.Locale exposing (l, lf, lg, lx)
import Shared.Markdown as Markdown
import Shared.Utils exposing (listFilterJust)
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


l_ : String -> AppState -> String
l_ =
    l "Wizard.Apps.Detail.View"


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Wizard.Apps.Detail.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Apps.Detail.View"


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
            a [ class "link-with-icon", onClick EditModalOpen, dataCy "app-detail_edit" ]
                [ faSet "_global.edit" appState
                , lx_ "actions.edit" appState
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
            , a [ onClick (DeletePlanModalOpen plan), class "text-danger ml-3", dataCy "app-detail_plan_delete" ] [ faSet "_global.delete" appState ]
            ]
    in
    DetailPage.content
        [ div [ DetailPage.contentInnerClass ]
            [ h3 [] [ lx_ "content.title.usage" appState ]
            , UsageTable.view appState appDetail.usage
            , hr [ class "my-5" ] []
            , h3 [] [ lx_ "content.title.plans" appState ]
            , PlansList.view appState { actions = Just planActions } appDetail.plans
            , a [ class "link-with-icon", onClick AddPlanModalOpen, dataCy "app-detail_add-plan" ]
                [ faSet "_global.add" appState, lx_ "content.plans.add" appState ]
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
                span [ class "badge badge-success" ] [ lx_ "badge.enabled" appState ]

            else
                span [ class "badge badge-danger" ] [ lx_ "badge.disabled" appState ]

        infoList =
            [ ( l_ "sidePanel.appId" appState, "app-id", text appDetail.appId )
            , ( l_ "sidePanel.enabled" appState, "enabled", enabledBadge )
            , ( l_ "sidePanel.createdAt" appState, "created-at", text <| TimeUtils.toReadableDateTime appState.timeZone appDetail.createdAt )
            , ( l_ "sidePanel.updatedAt" appState, "updated-at", text <| TimeUtils.toReadableDateTime appState.timeZone appDetail.updatedAt )
            ]
    in
    Just ( l_ "sidePanel.info" appState, "info", DetailPage.sidePanelList 4 8 infoList )


sidePanelUrls : AppState -> AppDetail -> Maybe ( String, String, Html msg )
sidePanelUrls appState appDetail =
    let
        urlsList =
            [ ( l_ "sidePanel.clientUrl" appState, "client-url", a [ href appDetail.clientUrl, target "_blank" ] [ text appDetail.clientUrl ] )
            , ( l_ "sidePanel.serverUrl" appState, "server-url", a [ href appDetail.serverUrl, target "_blank" ] [ text appDetail.serverUrl ] )
            ]
    in
    Just ( l_ "sidePanel.urls" appState, "urls", DetailPage.sidePanelList 4 8 urlsList )


sidePanelAdmins : AppState -> AppDetail -> Maybe ( String, String, Html msg )
sidePanelAdmins appState appDetail =
    let
        users =
            appDetail.users
                |> List.filter .active
                |> List.sortWith User.compare
                |> List.map viewUser
    in
    Just ( l_ "sidePanel.admins" appState, "admins", div [] users )


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
                    [ Html.map EditModalFormMsg <| FormGroup.input appState form "appId" <| lg "app.appId" appState
                    , Html.map EditModalFormMsg <| FormGroup.input appState form "name" <| lg "app.name" appState
                    ]

                Nothing ->
                    []

        config =
            { modalTitle = l_ "editModal.title" appState
            , modalContent = modalContent
            , visible = Maybe.isJust model.editForm
            , actionResult = model.savingApp
            , actionName = l_ "editModal.save" appState
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
                    [ Html.map AddPlanModalFormMsg <| FormGroup.input appState form "name" <| lg "appPlan.name" appState
                    , Html.map AddPlanModalFormMsg <| FormGroup.input appState form "users" <| lg "appPlan.users" appState
                    , Html.map AddPlanModalFormMsg <| FormGroup.simpleDate appState form "sinceYear" "sinceMonth" "sinceDay" <| lg "appPlan.from" appState
                    , Html.map AddPlanModalFormMsg <| FormGroup.simpleDate appState form "untilYear" "untilMonth" "untilDay" <| lg "appPlan.to" appState
                    , Html.map AddPlanModalFormMsg <| FormGroup.toggle form "test" <| lg "appPlan.trial" appState
                    ]

                Nothing ->
                    []

        config =
            { modalTitle = l_ "addPlanModal.title" appState
            , modalContent = modalContent
            , visible = Maybe.isJust model.addPlanForm
            , actionResult = model.addingPlan
            , actionName = l_ "addPlanModal.action" appState
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
                    [ Html.map EditPlanModalFormMsg <| FormGroup.input appState form "name" <| lg "appPlan.name" appState
                    , Html.map EditPlanModalFormMsg <| FormGroup.input appState form "users" <| lg "appPlan.users" appState
                    , Html.map EditPlanModalFormMsg <| FormGroup.simpleDate appState form "sinceYear" "sinceMonth" "sinceDay" <| lg "appPlan.from" appState
                    , Html.map EditPlanModalFormMsg <| FormGroup.simpleDate appState form "untilYear" "untilMonth" "untilDay" <| lg "appPlan.to" appState
                    , Html.map EditPlanModalFormMsg <| FormGroup.toggle form "test" <| lg "appPlan.trial" appState
                    ]

                Nothing ->
                    []

        config =
            { modalTitle = l_ "editPlanModal.title" appState
            , modalContent = modalContent
            , visible = Maybe.isJust model.editPlanForm
            , actionResult = model.editingPlan
            , actionName = l_ "editPlanModal.action" appState
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
                    [ Markdown.toHtml [] (lf_ "deletePlanModal.text" [ plan.name ] appState) ]

                Nothing ->
                    []

        config =
            { modalTitle = l_ "deletePlanModal.title" appState
            , modalContent = modalContent
            , visible = Maybe.isJust model.deletePlan
            , actionResult = model.deletingPlan
            , actionName = l_ "deletePlanModal.action" appState
            , actionMsg = DeletePlanModalConfirm
            , cancelMsg = Just DeletePlanModalClose
            , dangerous = True
            , dataCy = "app_plan-delete"
            }
    in
    Modal.confirm appState config
