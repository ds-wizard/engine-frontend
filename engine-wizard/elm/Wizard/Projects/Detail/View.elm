module Wizard.Projects.Detail.View exposing (view)

import ActionResult
import Html exposing (Html, button, div, li, p, span, text, ul)
import Html.Attributes exposing (attribute, class, classList)
import Html.Events exposing (onClick)
import Shared.Auth.Session as Session
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Html exposing (emptyNode, fa)
import Shared.Locale exposing (l, lx)
import Shared.Utils exposing (listInsertIf)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.OnlineUser as OnlineUser
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Common.Components.SummaryReport as SummaryReport
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.Page as Page
import Wizard.Projects.Common.QuestionnaireDescriptor as QuestionnaireDetail
import Wizard.Projects.Common.View exposing (visibilityIcons)
import Wizard.Projects.Detail.Components.NewDocument as NewDocument
import Wizard.Projects.Detail.Components.PlanSaving as PlanSaving
import Wizard.Projects.Detail.Components.Preview as Preview
import Wizard.Projects.Detail.Components.Settings as Settings
import Wizard.Projects.Detail.Components.ShareModal as ShareModal
import Wizard.Projects.Detail.Components.Todos as Todos
import Wizard.Projects.Detail.Documents.View as Documents
import Wizard.Projects.Detail.Models exposing (Model)
import Wizard.Projects.Detail.Msgs exposing (Msg(..))
import Wizard.Projects.Detail.PlanDetailRoute as PlanDetailRoute exposing (PlanDetailRoute)
import Wizard.Projects.Routes as PlansRoutes
import Wizard.Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Detail.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Projects.Detail.View"


view : PlanDetailRoute -> AppState -> Model -> Html Msg
view route appState model =
    if model.error then
        viewError appState

    else if model.offline then
        viewOffline appState

    else
        let
            actionResult =
                ActionResult.combine3
                    model.questionnaireModel
                    model.levels
                    model.metrics
        in
        Page.actionResultView appState (viewPlan route appState model) actionResult



-- ERROR PAGES


viewOffline : AppState -> Html Msg
viewOffline appState =
    Page.illustratedMessageHtml
        { image = "warning"
        , heading = l_ "offline.heading" appState
        , content =
            [ p [] [ lx_ "offline.text" appState ]
            , p [] [ button [ onClick Refresh, class "btn btn-lg btn-primary" ] [ lx_ "offline.refresh" appState ] ]
            ]
        }


viewError : AppState -> Html Msg
viewError appState =
    Page.illustratedMessageHtml
        { image = "warning"
        , heading = l_ "error.heading" appState
        , content =
            [ p [] [ lx_ "error.text" appState ]
            , p [] [ button [ onClick Refresh, class "btn btn-lg btn-primary" ] [ lx_ "error.refresh" appState ] ]
            ]
        }



-- PLAN


viewPlan : PlanDetailRoute -> AppState -> Model -> ( Questionnaire.Model, List Level, List Metric ) -> Html Msg
viewPlan route appState model ( qm, levels, metrics ) =
    div [ class "Plans__Detail" ]
        [ viewPlanNavigation appState route model qm
        , viewPlanContent appState route model qm levels metrics
        , Html.map ShareModalMsg <| ShareModal.view appState model.shareModalModel
        ]



-- PLAN - NAVIGATION


viewPlanNavigation : AppState -> PlanDetailRoute -> Model -> Questionnaire.Model -> Html Msg
viewPlanNavigation appState route model qm =
    div [ class "Plans__Detail__Navigation" ]
        [ viewPlanNavigationTitleRow appState model qm.questionnaire
        , viewPlanNavigationNav appState route model qm
        ]



-- PLAN - NAVIGATION - TITLE ROW


viewPlanNavigationTitleRow : AppState -> Model -> QuestionnaireDetail -> Html Msg
viewPlanNavigationTitleRow appState model questionnaire =
    div [ class "Plans__Detail__Navigation__Row" ]
        [ div [ class "Plans__Detail__Navigation__Row__Section" ]
            (div [ class "title" ] [ text questionnaire.name ]
                :: visibilityIcons appState questionnaire
                ++ [ viewPlanNavigationPlanSaving appState model ]
            )
        , div [ class "Plans__Detail__Navigation__Row__Section" ]
            [ viewPlanNavigationOnlineUsers appState model
            , viewPlanNavigationActions appState questionnaire
            ]
        ]


viewPlanNavigationPlanSaving : AppState -> Model -> Html Msg
viewPlanNavigationPlanSaving appState model =
    Html.map PlanSavingMsg <|
        PlanSaving.view appState model.planSavingModel


viewPlanNavigationOnlineUsers : AppState -> Model -> Html Msg
viewPlanNavigationOnlineUsers appState model =
    if List.isEmpty model.onlineUsers then
        emptyNode

    else
        let
            extraUsers =
                if List.length model.onlineUsers > 10 then
                    div [ class "extra-users-count" ]
                        [ text ("+" ++ String.fromInt (List.length model.onlineUsers - 10)) ]

                else
                    emptyNode
        in
        div
            [ class "Plans__Detail__Navigation__Row__Section__Online-Users"
            , classList [ ( "Plans__Detail__Navigation__Row__Section__Online-Users--Stacked", List.length model.onlineUsers > 5 ) ]
            ]
            (List.indexedMap (\i u -> Html.map (OnlineUserMsg i) (OnlineUser.view appState u)) (List.take 10 model.onlineUsers)
                ++ [ extraUsers ]
            )


viewPlanNavigationActions : AppState -> QuestionnaireDetail -> Html Msg
viewPlanNavigationActions appState questionnaire =
    if QuestionnaireDetail.isOwner appState questionnaire then
        div [ class "Plans__Detail__Navigation__Row__Section__Actions" ]
            [ button
                [ class "btn btn-info link-with-icon"
                , onClick (ShareModalMsg <| ShareModal.openMsg questionnaire)
                ]
                [ fa "fas fa-user-friends"
                , lx_ "actions.share" appState
                ]
            ]

    else
        emptyNode



-- PLAN - NAVIGATION - NAV ROW


viewPlanNavigationNav : AppState -> PlanDetailRoute -> Model -> Questionnaire.Model -> Html Msg
viewPlanNavigationNav appState route model qm =
    let
        isEditor =
            QuestionnaireDetail.isEditor appState qm.questionnaire

        isOwner =
            QuestionnaireDetail.isOwner appState qm.questionnaire

        isDocumentRoute r =
            case r of
                PlanDetailRoute.Documents _ ->
                    True

                PlanDetailRoute.NewDocument ->
                    True

                _ ->
                    False

        todosLength =
            QuestionnaireDetail.todosLength qm.questionnaire

        todosBadge =
            if todosLength > 0 then
                span [ class "badge badge-pill badge-danger" ] [ text (String.fromInt todosLength) ]

            else
                emptyNode

        questionnaireLink =
            li [ class "nav-item" ]
                [ linkTo appState
                    (Wizard.Routes.ProjectsRoute (PlansRoutes.DetailRoute model.uuid PlanDetailRoute.Questionnaire))
                    [ class "nav-link", classList [ ( "active", route == PlanDetailRoute.Questionnaire ) ] ]
                    [ fa "fa far fa-list-alt"
                    , span [ attribute "data-content" (l_ "nav.questionnaire" appState) ] [ lx_ "nav.questionnaire" appState ]
                    ]
                ]

        todosLink =
            li [ class "nav-item" ]
                [ linkTo appState
                    (Wizard.Routes.ProjectsRoute (PlansRoutes.DetailRoute model.uuid PlanDetailRoute.TODOs))
                    [ class "nav-link", classList [ ( "active", route == PlanDetailRoute.TODOs ) ] ]
                    [ fa "fa far fa-check-square"
                    , span [ attribute "data-content" (l_ "nav.todos" appState) ] [ lx_ "nav.todos" appState ]
                    , todosBadge
                    ]
                ]

        metricsLink =
            li [ class "nav-item" ]
                [ linkTo appState
                    (Wizard.Routes.ProjectsRoute (PlansRoutes.DetailRoute model.uuid PlanDetailRoute.Metrics))
                    [ class "nav-link", classList [ ( "active", route == PlanDetailRoute.Metrics ) ] ]
                    [ fa "fa far fa-chart-bar"
                    , span [ attribute "data-content" (l_ "nav.metrics" appState) ] [ lx_ "nav.metrics" appState ]
                    ]
                ]

        previewLink =
            li [ class "nav-item" ]
                [ linkTo appState
                    (Wizard.Routes.ProjectsRoute (PlansRoutes.DetailRoute model.uuid PlanDetailRoute.Preview))
                    [ class "nav-link", classList [ ( "active", route == PlanDetailRoute.Preview ) ] ]
                    [ fa "fa far fa-eye"
                    , span [ attribute "data-content" (l_ "nav.preview" appState) ] [ lx_ "nav.preview" appState ]
                    ]
                ]

        documentsLink =
            li [ class "nav-item" ]
                [ linkTo appState
                    (Wizard.Routes.ProjectsRoute (PlansRoutes.DetailRoute model.uuid (PlanDetailRoute.Documents PaginationQueryString.empty)))
                    [ class "nav-link", classList [ ( "active", isDocumentRoute route ) ] ]
                    [ fa "fa far fa-copy"
                    , span [ attribute "data-content" (l_ "nav.documents" appState) ] [ lx_ "nav.documents" appState ]
                    ]
                ]

        settingsLink =
            li [ class "nav-item" ]
                [ linkTo appState
                    (Wizard.Routes.ProjectsRoute (PlansRoutes.DetailRoute model.uuid PlanDetailRoute.Settings))
                    [ class "nav-link", classList [ ( "active", route == PlanDetailRoute.Settings ) ] ]
                    [ fa "fa fas fa-cogs"
                    , span [ attribute "data-content" (l_ "nav.settings" appState) ] [ lx_ "nav.settings" appState ]
                    ]
                ]

        links =
            []
                |> listInsertIf questionnaireLink True
                |> listInsertIf todosLink isEditor
                |> listInsertIf metricsLink appState.config.questionnaire.summaryReport.enabled
                |> listInsertIf previewLink True
                |> listInsertIf documentsLink True
                |> listInsertIf settingsLink isOwner
    in
    div [ class "Plans__Detail__Navigation__Row" ]
        [ ul [ class "nav nav-underline-tabs" ] links
        ]



-- PLAN - CONTENT


viewPlanContent : AppState -> PlanDetailRoute -> Model -> Questionnaire.Model -> List Level -> List Metric -> Html Msg
viewPlanContent appState route model qm levels metrics =
    let
        isEditable =
            QuestionnaireDetail.isEditor appState qm.questionnaire

        isAuthenticated =
            Session.exists appState.session

        forbiddenPage =
            Page.error appState (l_ "forbidden" appState)
    in
    case route of
        PlanDetailRoute.Questionnaire ->
            Html.map QuestionnaireMsg <|
                Questionnaire.view appState
                    { features =
                        { feedbackEnabled = True
                        , todosEnabled = isEditable
                        , readonly = not isEditable
                        }
                    , renderer = DefaultQuestionnaireRenderer.create appState qm.questionnaire.knowledgeModel levels metrics
                    }
                    { levels = levels, metrics = metrics, events = [] }
                    qm

        PlanDetailRoute.Preview ->
            Html.map PreviewMsg <|
                Preview.view appState qm.questionnaire model.previewModel

        PlanDetailRoute.TODOs ->
            if isEditable then
                Todos.view appState qm.questionnaire

            else
                forbiddenPage

        PlanDetailRoute.Metrics ->
            Html.map SummaryReportMsg <|
                SummaryReport.view appState { questionnaire = qm.questionnaire, metrics = metrics } model.summaryReportModel

        PlanDetailRoute.Documents _ ->
            Html.map DocumentsMsg <|
                Documents.view appState { questionnaireUuid = qm.questionnaire.uuid, questionnaireEditable = isEditable } model.documentsModel

        PlanDetailRoute.NewDocument ->
            if isEditable then
                Html.map NewDocumentMsg <|
                    NewDocument.view appState qm.questionnaire model.newDocumentModel

            else
                forbiddenPage

        PlanDetailRoute.Settings ->
            if isEditable && isAuthenticated then
                Html.map SettingsMsg <|
                    Settings.view appState { questionnaire = QuestionnaireDetail.fromQuestionnaireDetail qm.questionnaire } model.settingsModel

            else
                forbiddenPage
