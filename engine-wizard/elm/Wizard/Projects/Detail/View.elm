module Wizard.Projects.Detail.View exposing (view)

import Html exposing (Html, button, div, p, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Shared.Auth.Session as Session
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Html exposing (emptyNode, fa)
import Shared.Locale exposing (l, lgx, lx)
import Shared.Undraw as Undraw
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Common.Components.ActionResultView as ActionResultView
import Wizard.Common.Components.DetailNavigation as DetailNavigation
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Common.Components.SummaryReport as SummaryReport
import Wizard.Common.Feature as Features
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.Page as Page
import Wizard.Projects.Common.QuestionnaireDescriptor as QuestionnaireDescriptor
import Wizard.Projects.Common.View exposing (visibilityIcons)
import Wizard.Projects.Detail.Components.NewDocument as NewDocument
import Wizard.Projects.Detail.Components.Preview as Preview
import Wizard.Projects.Detail.Components.ProjectSaving as ProjectSaving
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
        { image = Undraw.warning
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
        { image = Undraw.warning
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
    DetailNavigation.container
        [ viewProjectNavigationTitleRow appState model qm.questionnaire
        , viewProjectNavigationNav appState route model qm
        ]



-- PROJECT - NAVIGATION - TITLE ROW


viewProjectNavigationTitleRow : AppState -> Model -> QuestionnaireDetail -> Html Msg
viewProjectNavigationTitleRow appState model questionnaire =
    DetailNavigation.row
        [ DetailNavigation.section
            (div [ class "title" ] [ text questionnaire.name ]
                :: templateBadge appState questionnaire
                :: visibilityIcons appState questionnaire
                ++ [ viewProjectNavigationProjectSaving appState model ]
            )
        , DetailNavigation.section
            [ DetailNavigation.onlineUsers OnlineUserMsg appState model.onlineUsers
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
    Html.map ProjectSavingMsg <|
        ProjectSaving.view appState model.projectSavingModel


viewProjectNavigationActions : AppState -> Model -> QuestionnaireDetail -> Html Msg
viewProjectNavigationActions appState model questionnaire =
    if QuestionnaireDetail.isAnonymousProject questionnaire && Session.exists appState.session then
        DetailNavigation.sectionActions
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
        DetailNavigation.sectionActions
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
        projectRoute subroute =
            Wizard.Routes.ProjectsRoute (PlansRoutes.DetailRoute model.uuid subroute)

        questionnaire =
            qm.questionnaire

        isDocumentRoute =
            case route of
                ProjectDetailRoute.Documents _ ->
                    True

                ProjectDetailRoute.NewDocument _ ->
                    True

                _ ->
                    False

        questionnaireLink =
            { route = projectRoute ProjectDetailRoute.Questionnaire
            , label = l_ "nav.questionnaire" appState
            , icon = fa "fa far fa-list-alt"
            , isActive = route == ProjectDetailRoute.Questionnaire
            , isVisible = True
            , dataCy = "project_nav_questionnaire"
            }

        metricsLink =
            { route = projectRoute ProjectDetailRoute.Metrics
            , label = l_ "nav.metrics" appState
            , icon = fa "fa far fa-chart-bar"
            , isActive = route == ProjectDetailRoute.Metrics
            , isVisible = Features.projectMetrics appState questionnaire
            , dataCy = "project_nav_metrics"
            }

        previewLink =
            { route = projectRoute ProjectDetailRoute.Preview
            , label = l_ "nav.preview" appState
            , icon = fa "fa far fa-eye"
            , isActive = route == ProjectDetailRoute.Preview
            , isVisible = Features.projectPreview appState questionnaire
            , dataCy = "project_nav_preview"
            }

        documentsLink =
            { route = projectRoute (ProjectDetailRoute.Documents PaginationQueryString.empty)
            , label = l_ "nav.documents" appState
            , icon = fa "fa far fa-copy"
            , isActive = isDocumentRoute
            , isVisible = Features.projectDocumentsView appState questionnaire
            , dataCy = "project_nav_documents"
            }

        settingsLink =
            { route = projectRoute ProjectDetailRoute.Settings
            , label = l_ "nav.settings" appState
            , icon = fa "fa fas fa-cogs"
            , isActive = route == ProjectDetailRoute.Settings
            , isVisible = Features.projectSettings appState questionnaire
            , dataCy = "project_nav_settings"
            }

        links =
            [ questionnaireLink
            , metricsLink
            , previewLink
            , documentsLink
            , settingsLink
            ]
    in
    DetailNavigation.navigation appState links



-- PROJECT - CONTENT


viewProjectContent : AppState -> ProjectDetailRoute -> Model -> Questionnaire.Model -> Html Msg
viewProjectContent appState route model qm =
    let
        isEditable =
            QuestionnaireDetail.isEditor appState qm.questionnaire

        isAuthenticated =
            Session.exists appState.session

        forbiddenPage =
            Page.error appState (l_ "forbidden" appState)
    in
    case route of
        ProjectDetailRoute.Questionnaire ->
            let
                isMigrating =
                    QuestionnaireDetail.isMigrating qm.questionnaire
            in
            Questionnaire.view appState
                { features =
                    { feedbackEnabled = True
                    , todosEnabled = isEditable
                    , commentsEnabled = True
                    , readonly = not isEditable || isMigrating
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
            let
                isOwner =
                    QuestionnaireDetail.isOwner appState qm.questionnaire
            in
            if isOwner && isAuthenticated then
                Html.map SettingsMsg <|
                    Settings.view appState
                        { questionnaire = QuestionnaireDescriptor.fromQuestionnaireDetail qm.questionnaire
                        , package = qm.questionnaire.package
                        , templateState = qm.questionnaire.templateState
                        }
                        model.settingsModel

            else
                forbiddenPage
