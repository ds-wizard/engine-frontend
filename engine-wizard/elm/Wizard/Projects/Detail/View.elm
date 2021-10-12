module Wizard.Projects.Detail.View exposing (view)

import Html exposing (Html, button, div, li, p, span, text, ul)
import Html.Attributes exposing (attribute, class, classList)
import Html.Events exposing (onClick)
import Shared.Auth.Session as Session
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Html exposing (emptyNode, fa)
import Shared.Locale exposing (l, lgx, lx)
import Shared.Utils exposing (listInsertIf)
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Common.Components.ActionResultView as ActionResultView
import Wizard.Common.Components.OnlineUser as OnlineUser
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Common.Components.SummaryReport as SummaryReport
import Wizard.Common.Feature as Features
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.Page as Page
import Wizard.Projects.Common.QuestionnaireDescriptor as QuestionnaireDetail
import Wizard.Projects.Common.View exposing (visibilityIcons)
import Wizard.Projects.Detail.Components.NewDocument as NewDocument
import Wizard.Projects.Detail.Components.PlanSaving as PlanSaving
import Wizard.Projects.Detail.Components.Preview as Preview
import Wizard.Projects.Detail.Components.QuestionnaireVersionViewModal as QuestionnaireVersionViewModal
import Wizard.Projects.Detail.Components.RevertModal as ReverModal
import Wizard.Projects.Detail.Components.Settings as Settings
import Wizard.Projects.Detail.Components.ShareModal as ShareModal
import Wizard.Projects.Detail.Documents.View as Documents
import Wizard.Projects.Detail.Models exposing (Model)
import Wizard.Projects.Detail.Msgs exposing (Msg(..))
import Wizard.Projects.Detail.ProjectDetailRoute as ProjectDetailRoute exposing (ProjectDetailRoute)
import Wizard.Projects.Routes as PlansRoutes
import Wizard.Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Detail.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Projects.Detail.View"


view : ProjectDetailRoute -> AppState -> Model -> Html Msg
view route appState model =
    if model.error then
        viewError appState

    else if model.offline then
        viewOffline appState

    else
        Page.actionResultView appState (viewProject route appState model) model.questionnaireModel



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
        , cy = "offline"
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
        , cy = "error"
        }



-- PROJECT


viewProject : ProjectDetailRoute -> AppState -> Model -> Questionnaire.Model -> Html Msg
viewProject route appState model qm =
    let
        navigation =
            if AppState.isFullscreen appState then
                emptyNode

            else
                viewProjectNavigation appState route model qm

        modalConfig =
            { events = qm.questionnaire.events
            , versions = qm.questionnaire.versions
            }
    in
    div [ class "Projects__Detail" ]
        [ navigation
        , viewProjectContent appState route model qm
        , Html.map ShareModalMsg <| ShareModal.view appState model.shareModalModel
        , Html.map QuestionnaireVersionViewModalMsg <| QuestionnaireVersionViewModal.view modalConfig appState model.questionnaireVersionViewModalModel
        , Html.map RevertModalMsg <| ReverModal.view appState model.revertModalModel
        ]



-- PROJECT - NAVIGATION


viewProjectNavigation : AppState -> ProjectDetailRoute -> Model -> Questionnaire.Model -> Html Msg
viewProjectNavigation appState route model qm =
    div [ class "DetailNavigation" ]
        [ viewProjectNavigationTitleRow appState model qm.questionnaire
        , viewProjectNavigationNav appState route model qm
        ]



-- PROJECT - NAVIGATION - TITLE ROW


viewProjectNavigationTitleRow : AppState -> Model -> QuestionnaireDetail -> Html Msg
viewProjectNavigationTitleRow appState model questionnaire =
    div [ class "DetailNavigation__Row" ]
        [ div [ class "DetailNavigation__Row__Section" ]
            (div [ class "title" ] [ text questionnaire.name ]
                :: templateBadge appState questionnaire
                :: visibilityIcons appState questionnaire
                ++ [ viewProjectNavigationProjectSaving appState model ]
            )
        , div [ class "DetailNavigation__Row__Section" ]
            [ viewProjectNavigationOnlineUsers appState model
            , viewProjectNavigationActions appState model questionnaire
            ]
        ]


templateBadge : AppState -> QuestionnaireDetail -> Html msg
templateBadge appState questionnaire =
    if questionnaire.isTemplate then
        span [ class "badge badge-info" ]
            [ lgx "questionnaire.templateBadge" appState ]

    else
        emptyNode


viewProjectNavigationProjectSaving : AppState -> Model -> Html Msg
viewProjectNavigationProjectSaving appState model =
    Html.map PlanSavingMsg <|
        PlanSaving.view appState model.planSavingModel


viewProjectNavigationOnlineUsers : AppState -> Model -> Html Msg
viewProjectNavigationOnlineUsers appState model =
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
            [ class "DetailNavigation__Row__Section__Online-Users"
            , classList [ ( "DetailNavigation__Row__Section__Online-Users--Stacked", List.length model.onlineUsers > 5 ) ]
            ]
            (List.indexedMap (\i u -> Html.map (OnlineUserMsg i) (OnlineUser.view appState u)) (List.take 10 model.onlineUsers)
                ++ [ extraUsers ]
            )


viewProjectNavigationActions : AppState -> Model -> QuestionnaireDetail -> Html Msg
viewProjectNavigationActions appState model questionnaire =
    if QuestionnaireDetail.isAnonymousProject questionnaire && Session.exists appState.session then
        div [ class "DetailNavigation__Row__Section__Actions" ]
            [ ActionResultView.error model.addingToMyProjects
            , ActionButton.buttonExtra appState
                { content =
                    [ fa "fas fa-plus"
                    , lx_ "actions.add" appState
                    ]
                , result = model.addingToMyProjects
                , msg = AddToMyProjects
                , dangerous = False
                }
            ]

    else if QuestionnaireDetail.isOwner appState questionnaire then
        div [ class "DetailNavigation__Row__Section__Actions" ]
            [ button
                [ class "btn btn-info link-with-icon"
                , onClick (ShareModalMsg <| ShareModal.openMsg questionnaire)
                , dataCy "project_detail_share-button"
                ]
                [ fa "fas fa-user-friends"
                , lx_ "actions.share" appState
                ]
            ]

    else
        emptyNode



-- PROJECT - NAVIGATION - NAV ROW


viewProjectNavigationNav : AppState -> ProjectDetailRoute -> Model -> Questionnaire.Model -> Html Msg
viewProjectNavigationNav appState route model qm =
    let
        questionnaire =
            qm.questionnaire

        isDocumentRoute r =
            case r of
                ProjectDetailRoute.Documents _ ->
                    True

                ProjectDetailRoute.NewDocument _ ->
                    True

                _ ->
                    False

        questionnaireLink =
            li [ class "nav-item" ]
                [ linkTo appState
                    (Wizard.Routes.ProjectsRoute (PlansRoutes.DetailRoute model.uuid ProjectDetailRoute.Questionnaire))
                    [ class "nav-link", classList [ ( "active", route == ProjectDetailRoute.Questionnaire ) ] ]
                    [ fa "fa far fa-list-alt"
                    , span [ attribute "data-content" (l_ "nav.questionnaire" appState) ] [ lx_ "nav.questionnaire" appState ]
                    ]
                ]

        metricsLink =
            li [ class "nav-item" ]
                [ linkTo appState
                    (Wizard.Routes.ProjectsRoute (PlansRoutes.DetailRoute model.uuid ProjectDetailRoute.Metrics))
                    [ class "nav-link", classList [ ( "active", route == ProjectDetailRoute.Metrics ) ] ]
                    [ fa "fa far fa-chart-bar"
                    , span [ attribute "data-content" (l_ "nav.metrics" appState) ] [ lx_ "nav.metrics" appState ]
                    ]
                ]

        metricsLinkVisible =
            Features.projectMetrics appState questionnaire

        previewLink =
            li [ class "nav-item" ]
                [ linkTo appState
                    (Wizard.Routes.ProjectsRoute (PlansRoutes.DetailRoute model.uuid ProjectDetailRoute.Preview))
                    [ class "nav-link", classList [ ( "active", route == ProjectDetailRoute.Preview ) ] ]
                    [ fa "fa far fa-eye"
                    , span [ attribute "data-content" (l_ "nav.preview" appState) ] [ lx_ "nav.preview" appState ]
                    ]
                ]

        previewLinkVisible =
            Features.projectPreview appState questionnaire

        documentsLink =
            li [ class "nav-item" ]
                [ linkTo appState
                    (Wizard.Routes.ProjectsRoute (PlansRoutes.DetailRoute model.uuid (ProjectDetailRoute.Documents PaginationQueryString.empty)))
                    [ class "nav-link", classList [ ( "active", isDocumentRoute route ) ] ]
                    [ fa "fa far fa-copy"
                    , span [ attribute "data-content" (l_ "nav.documents" appState) ] [ lx_ "nav.documents" appState ]
                    ]
                ]

        documentsLinkVisible =
            Features.projectDocumentsView appState questionnaire

        settingsLink =
            li [ class "nav-item" ]
                [ linkTo appState
                    (Wizard.Routes.ProjectsRoute (PlansRoutes.DetailRoute model.uuid ProjectDetailRoute.Settings))
                    [ class "nav-link", classList [ ( "active", route == ProjectDetailRoute.Settings ) ] ]
                    [ fa "fa fas fa-cogs"
                    , span [ attribute "data-content" (l_ "nav.settings" appState) ] [ lx_ "nav.settings" appState ]
                    ]
                ]

        settingsLinkVisible =
            Features.projectSettings appState questionnaire

        links =
            []
                |> listInsertIf questionnaireLink True
                |> listInsertIf metricsLink metricsLinkVisible
                |> listInsertIf previewLink previewLinkVisible
                |> listInsertIf documentsLink documentsLinkVisible
                |> listInsertIf settingsLink settingsLinkVisible
    in
    div [ class "DetailNavigation__Row" ]
        [ ul [ class "nav nav-underline-tabs" ] links
        ]



-- PROJECT - CONTENT


viewProjectContent : AppState -> ProjectDetailRoute -> Model -> Questionnaire.Model -> Html Msg
viewProjectContent appState route model qm =
    let
        isOwner =
            QuestionnaireDetail.isOwner appState qm.questionnaire

        isEditable =
            QuestionnaireDetail.isEditor appState qm.questionnaire

        isAuthenticated =
            Session.exists appState.session

        forbiddenPage =
            Page.error appState (l_ "forbidden" appState)
    in
    case route of
        ProjectDetailRoute.Questionnaire ->
            Questionnaire.view appState
                { features =
                    { feedbackEnabled = True
                    , todosEnabled = isEditable
                    , commentsEnabled = True
                    , readonly = not isEditable
                    , toolbarEnabled = True
                    }
                , renderer = DefaultQuestionnaireRenderer.create appState qm.questionnaire.knowledgeModel
                , wrapMsg = QuestionnaireMsg
                , previewQuestionnaireEventMsg = Just (OpenVersionPreview qm.questionnaire.uuid)
                , revertQuestionnaireMsg = Just OpenRevertModal
                }
                { events = [] }
                qm

        ProjectDetailRoute.Preview ->
            Html.map PreviewMsg <|
                Preview.view appState qm.questionnaire model.previewModel

        ProjectDetailRoute.Metrics ->
            Html.map SummaryReportMsg <|
                SummaryReport.view appState { questionnaire = qm.questionnaire } model.summaryReportModel

        ProjectDetailRoute.Documents _ ->
            Documents.view appState
                { questionnaire = qm.questionnaire
                , questionnaireEditable = isEditable
                , wrapMsg = DocumentsMsg
                , previewQuestionnaireEventMsg = Just (OpenVersionPreview qm.questionnaire.uuid)
                }
                model.documentsModel

        ProjectDetailRoute.NewDocument mbEventUuid ->
            if isEditable && isAuthenticated then
                Html.map NewDocumentMsg <|
                    NewDocument.view appState qm.questionnaire mbEventUuid model.newDocumentModel

            else
                forbiddenPage

        ProjectDetailRoute.Settings ->
            if isOwner && isAuthenticated then
                Html.map SettingsMsg <|
                    Settings.view appState
                        { questionnaire = QuestionnaireDetail.fromQuestionnaireDetail qm.questionnaire, package = qm.questionnaire.package }
                        model.settingsModel

            else
                forbiddenPage
