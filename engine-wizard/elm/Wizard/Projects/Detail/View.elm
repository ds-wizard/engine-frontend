module Wizard.Projects.Detail.View exposing (view)

import ActionResult
import Gettext exposing (gettext)
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Shared.Auth.Session as Session
import Shared.Components.Badge as Badge
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.QuestionnaireCommon exposing (QuestionnaireCommon)
import Shared.Html exposing (emptyNode, fa, faSet)
import Shared.Undraw as Undraw
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Common.Components.ActionResultView as ActionResultView
import Wizard.Common.Components.DetailNavigation as DetailNavigation
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Common.Components.SummaryReport as SummaryReport
import Wizard.Common.Feature as Features
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.QuestionnaireUtils as QuestionnaireUtils
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.Page as Page
import Wizard.Projects.Common.View exposing (visibilityIcons)
import Wizard.Projects.Detail.Components.NewDocument as NewDocument
import Wizard.Projects.Detail.Components.Preview as Preview
import Wizard.Projects.Detail.Components.ProjectSaving as ProjectSaving
import Wizard.Projects.Detail.Components.QuestionnaireVersionViewModal as QuestionnaireVersionViewModal
import Wizard.Projects.Detail.Components.RevertModal as RevertModal
import Wizard.Projects.Detail.Components.Settings as Settings
import Wizard.Projects.Detail.Components.ShareModal as ShareModal
import Wizard.Projects.Detail.Documents.View as Documents
import Wizard.Projects.Detail.Models exposing (Model)
import Wizard.Projects.Detail.Msgs exposing (Msg(..))
import Wizard.Projects.Detail.ProjectDetailRoute as ProjectDetailRoute exposing (ProjectDetailRoute)
import Wizard.Projects.Routes as PlansRoutes
import Wizard.Routes


view : ProjectDetailRoute -> AppState -> Model -> Html Msg
view route appState model =
    if model.error then
        viewError appState

    else if model.offline then
        viewOffline appState

    else
        Page.actionResultView appState (viewProject route appState model) model.questionnaireCommon



-- ERROR PAGES


viewOffline : AppState -> Html Msg
viewOffline appState =
    Page.illustratedMessageHtml
        { image = Undraw.warning
        , heading = gettext "Disconnected" appState.locale
        , content =
            [ p [] [ text (gettext "You have been disconnected, try to refresh the page." appState.locale) ]
            , p [] [ button [ onClick Refresh, class "btn btn-lg btn-primary" ] [ text (gettext "Refresh" appState.locale) ] ]
            ]
        , cy = "offline"
        }


viewError : AppState -> Html Msg
viewError appState =
    Page.illustratedMessageHtml
        { image = Undraw.warning
        , heading = gettext "Oops!" appState.locale
        , content =
            [ p [] [ text (gettext "Something went wrong, try to refresh the page." appState.locale) ]
            , p [] [ button [ onClick Refresh, class "btn btn-lg btn-primary" ] [ text (gettext "Refresh" appState.locale) ] ]
            ]
        , cy = "error"
        }



-- PROJECT


viewProject : ProjectDetailRoute -> AppState -> Model -> QuestionnaireCommon -> Html Msg
viewProject route appState model questionnaire =
    let
        navigation =
            if AppState.isFullscreen appState then
                emptyNode

            else
                viewProjectNavigation appState route model questionnaire

        modalConfig =
            { events =
                model.questionnaireModel
                    |> ActionResult.andThen .questionnaireEvents
                    |> ActionResult.withDefault []
            , versions =
                model.questionnaireModel
                    |> ActionResult.andThen .questionnaireVersions
                    |> ActionResult.withDefault []
            }
    in
    div [ class "Projects__Detail col-full flex-column" ]
        [ navigation
        , viewProjectContent appState route model questionnaire
        , Html.map ShareModalMsg <| ShareModal.view appState model.shareModalModel
        , Html.map QuestionnaireVersionViewModalMsg <| QuestionnaireVersionViewModal.view modalConfig appState model.questionnaireVersionViewModalModel
        , Html.map RevertModalMsg <| RevertModal.view appState model.revertModalModel
        ]



-- PROJECT - NAVIGATION


viewProjectNavigation : AppState -> ProjectDetailRoute -> Model -> QuestionnaireCommon -> Html Msg
viewProjectNavigation appState route model questionnaire =
    DetailNavigation.container
        [ viewProjectNavigationTitleRow appState model questionnaire
        , viewProjectNavigationNav appState route model questionnaire
        ]



-- PROJECT - NAVIGATION - TITLE ROW


viewProjectNavigationTitleRow : AppState -> Model -> QuestionnaireCommon -> Html Msg
viewProjectNavigationTitleRow appState model questionnaire =
    DetailNavigation.row
        [ DetailNavigation.section
            (div [ class "title" ] [ text questionnaire.name ]
                :: templateBadge appState questionnaire
                :: visibilityIcons appState questionnaire
                ++ [ viewProjectNavigationProjectSaving appState model ]
            )
        , DetailNavigation.section
            [ DetailNavigation.onlineUsers appState model.onlineUsers
            , viewProjectNavigationActions appState model questionnaire
            ]
        ]


templateBadge : AppState -> QuestionnaireCommon -> Html msg
templateBadge appState questionnaire =
    if questionnaire.isTemplate then
        Badge.info [] [ text (gettext "Template" appState.locale) ]

    else
        emptyNode


viewProjectNavigationProjectSaving : AppState -> Model -> Html Msg
viewProjectNavigationProjectSaving appState model =
    Html.map ProjectSavingMsg <|
        ProjectSaving.view appState model.projectSavingModel


viewProjectNavigationActions : AppState -> Model -> QuestionnaireCommon -> Html Msg
viewProjectNavigationActions appState model questionnaire =
    if QuestionnaireUtils.isAnonymousProject questionnaire && Session.exists appState.session then
        DetailNavigation.sectionActions
            [ ActionResultView.error model.addingToMyProjects
            , ActionButton.buttonCustom appState
                { content =
                    [ fa "fas fa-plus"
                    , text (gettext "Add to my projects" appState.locale)
                    ]
                , result = model.addingToMyProjects
                , msg = AddToMyProjects
                , btnClass = "btn-primary with-icon"
                }
            ]

    else if QuestionnaireUtils.isOwner appState questionnaire then
        DetailNavigation.sectionActions
            [ button
                [ class "btn btn-info text-light with-icon"
                , onClick (ShareModalMsg <| ShareModal.openMsg questionnaire)
                , dataCy "project_detail_share-button"
                ]
                [ fa "fas fa-user-friends"
                , text (gettext "Share" appState.locale)
                ]
            ]

    else
        emptyNode



-- PROJECT - NAVIGATION - NAV ROW


viewProjectNavigationNav : AppState -> ProjectDetailRoute -> Model -> QuestionnaireCommon -> Html Msg
viewProjectNavigationNav appState route model questionnaire =
    let
        projectRoute subroute =
            Wizard.Routes.ProjectsRoute (PlansRoutes.DetailRoute model.uuid subroute)

        isQuestionnaireRoute =
            case route of
                ProjectDetailRoute.Questionnaire _ ->
                    True

                _ ->
                    False

        isDocumentRoute =
            case route of
                ProjectDetailRoute.Documents _ ->
                    True

                ProjectDetailRoute.NewDocument _ ->
                    True

                _ ->
                    False

        questionnaireLink =
            { route = projectRoute (ProjectDetailRoute.Questionnaire Nothing)
            , label = gettext "Questionnaire" appState.locale
            , icon = faSet "project.questionnaire" appState
            , isActive = isQuestionnaireRoute
            , isVisible = True
            , dataCy = "project_nav_questionnaire"
            }

        metricsLink =
            { route = projectRoute ProjectDetailRoute.Metrics
            , label = gettext "Metrics" appState.locale
            , icon = faSet "project.metrics" appState
            , isActive = route == ProjectDetailRoute.Metrics
            , isVisible = Features.projectMetrics appState
            , dataCy = "project_nav_metrics"
            }

        previewLink =
            { route = projectRoute ProjectDetailRoute.Preview
            , label = gettext "Preview" appState.locale
            , icon = faSet "_global.preview" appState
            , isActive = route == ProjectDetailRoute.Preview
            , isVisible = Features.projectPreview appState
            , dataCy = "project_nav_preview"
            }

        documentsLink =
            { route = projectRoute (ProjectDetailRoute.Documents PaginationQueryString.empty)
            , label = gettext "Documents" appState.locale
            , icon = faSet "project.documents" appState
            , isActive = isDocumentRoute
            , isVisible = Features.projectDocumentsView appState
            , dataCy = "project_nav_documents"
            }

        settingsLink =
            { route = projectRoute ProjectDetailRoute.Settings
            , label = gettext "Settings" appState.locale
            , icon = faSet "_global.settings" appState
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


viewProjectContent : AppState -> ProjectDetailRoute -> Model -> QuestionnaireCommon -> Html Msg
viewProjectContent appState route model questionnaire =
    let
        isEditable =
            QuestionnaireUtils.isEditor appState questionnaire

        isAuthenticated =
            Session.exists appState.session

        forbiddenPage =
            Page.error appState (gettext "You are not allowed to view this page." appState.locale)
    in
    case route of
        ProjectDetailRoute.Questionnaire _ ->
            let
                viewContent qm =
                    let
                        isMigrating =
                            QuestionnaireUtils.isMigrating qm.questionnaire
                    in
                    Questionnaire.view appState
                        { features =
                            { feedbackEnabled = True
                            , todosEnabled = isEditable
                            , commentsEnabled = True
                            , readonly = not isEditable || isMigrating
                            , toolbarEnabled = True
                            , questionLinksEnabled = True
                            }
                        , renderer = DefaultQuestionnaireRenderer.create appState qm.questionnaire.knowledgeModel
                        , wrapMsg = QuestionnaireMsg
                        , previewQuestionnaireEventMsg = Just (OpenVersionPreview qm.questionnaire.uuid)
                        , revertQuestionnaireMsg = Just OpenRevertModal
                        }
                        { events = [] }
                        qm
            in
            Page.actionResultView appState viewContent model.questionnaireModel

        ProjectDetailRoute.Preview ->
            let
                viewContent questionnairePreview =
                    Html.map PreviewMsg <|
                        Preview.view appState questionnairePreview model.previewModel
            in
            Page.actionResultView appState viewContent model.questionnairePreview

        ProjectDetailRoute.Metrics ->
            let
                viewContent summaryReport =
                    Html.map SummaryReportMsg <|
                        SummaryReport.view appState summaryReport
            in
            Page.actionResultView appState viewContent model.questionnaireSummaryReport

        ProjectDetailRoute.Documents _ ->
            Documents.view appState
                { questionnaire = questionnaire
                , questionnaireEditable = isEditable
                , wrapMsg = DocumentsMsg
                , previewQuestionnaireEventMsg = Just (OpenVersionPreview questionnaire.uuid)
                }
                model.documentsModel

        ProjectDetailRoute.NewDocument _ ->
            if isEditable && isAuthenticated then
                Html.map NewDocumentMsg <|
                    NewDocument.view appState questionnaire model.newDocumentModel

            else
                forbiddenPage

        ProjectDetailRoute.Settings ->
            let
                isOwner =
                    QuestionnaireUtils.isOwner appState questionnaire

                viewContent questionnaireSettings =
                    Html.map SettingsMsg <|
                        Settings.view appState questionnaireSettings model.settingsModel
            in
            if isOwner && isAuthenticated then
                Page.actionResultView appState viewContent model.questionnaireSettings

            else
                forbiddenPage
