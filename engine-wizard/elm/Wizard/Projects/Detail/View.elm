module Wizard.Projects.Detail.View exposing (view)

import ActionResult
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Gettext exposing (gettext)
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Shared.Auth.Session as Session
import Shared.Components.Badge as Badge
import Shared.Components.FontAwesome exposing (fa, faPreview, faProjectDocuments, faProjectFiles, faProjectMetrics, faProjectQuestionnaire, faQuestionnaireCopyLink, faSettings)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Undraw as Undraw
import Shared.Utils exposing (flip)
import String.Format as String
import Wizard.Api.Models.QuestionnaireCommon exposing (QuestionnaireCommon)
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Common.Components.ActionResultView as ActionResultView
import Wizard.Common.Components.DetailNavigation as DetailNavigation
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.DefaultQuestionnaireRenderer as DefaultQuestionnaireRenderer
import Wizard.Common.Components.SummaryReport as SummaryReport
import Wizard.Common.Feature as Features
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, dataTour)
import Wizard.Common.QuestionnaireUtils as QuestionnaireUtils
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Projects.Common.View exposing (shareIcon, shareTooltipHtml)
import Wizard.Projects.Detail.Components.NewDocument as NewDocument
import Wizard.Projects.Detail.Components.Preview as Preview
import Wizard.Projects.Detail.Components.ProjectSaving as ProjectSaving
import Wizard.Projects.Detail.Components.QuestionnaireVersionViewModal as QuestionnaireVersionViewModal
import Wizard.Projects.Detail.Components.RevertModal as RevertModal
import Wizard.Projects.Detail.Components.Settings as Settings
import Wizard.Projects.Detail.Components.ShareModal as ShareModal
import Wizard.Projects.Detail.Documents.View as Documents
import Wizard.Projects.Detail.Files.View as Files
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
        ( migrationWarning, migrationWarningEnabled ) =
            case questionnaire.migrationUuid of
                Just migrationUuid ->
                    let
                        warningLink =
                            linkTo (Wizard.Routes.projectsMigration migrationUuid) [] [ text (gettext "project migration" appState.locale) ]

                        warningContent =
                            gettext "There is an ongoing %s. Finish it before you can continue editing this project." appState.locale
                                |> flip String.formatHtml [ warningLink ]
                    in
                    ( div [ class "Projects__Detail__Warning" ] [ div [] warningContent ]
                    , True
                    )

                Nothing ->
                    ( Html.nothing, False )

        navigation =
            if AppState.isFullscreen appState then
                Html.nothing

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
    div
        [ class "Projects__Detail col-full flex-column"
        , classList [ ( "Projects__Detail--Warning", migrationWarningEnabled ) ]
        ]
        [ migrationWarning
        , navigation
        , viewProjectContent appState route model questionnaire
        , Html.map ShareModalMsg <| ShareModal.view appState model.shareModalModel
        , Html.map QuestionnaireVersionViewModalMsg <| QuestionnaireVersionViewModal.view modalConfig appState model.questionnaireVersionViewModalModel
        , Html.map RevertModalMsg <| RevertModal.view appState model.revertModalModel
        , disconnectedModal appState model
        ]


disconnectedModal : AppState -> Model -> Html Msg
disconnectedModal appState model =
    let
        modalContent =
            [ p [] [ text (gettext "Another user has made significant changes to the questionnaire (e.g., reverting to a previous version). To ensure everyone is using the latest version, you’ve been disconnected." appState.locale) ]
            , p [] [ text (gettext "Refresh the page to reconnect." appState.locale) ]
            ]

        cfg =
            Modal.confirmConfig (gettext "Refresh Required" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible (model.forceDisconnect && (model.revertModalModel.revertResult /= ActionResult.Loading))
                |> Modal.confirmConfigAction (gettext "Refresh" appState.locale) Refresh
                |> Modal.confirmConfigDataCy "disconnected_modal"
    in
    Modal.confirm appState cfg



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
    let
        isTooltipLeft =
            not (QuestionnaireUtils.isAnonymousProject questionnaire && Session.exists appState.session) && not (QuestionnaireUtils.isOwner appState questionnaire)
    in
    DetailNavigation.row
        [ DetailNavigation.section
            [ div [ class "title" ] [ text questionnaire.name ]
            , templateBadge appState questionnaire
            , viewProjectNavigationProjectSaving appState model
            ]
        , DetailNavigation.section
            [ DetailNavigation.onlineUsers appState isTooltipLeft model.onlineUsers
            , viewProjectNavigationActions appState model questionnaire
            ]
        ]


templateBadge : AppState -> QuestionnaireCommon -> Html msg
templateBadge appState questionnaire =
    if questionnaire.isTemplate then
        Badge.info [] [ text (gettext "Template" appState.locale) ]

    else
        Html.nothing


viewProjectNavigationProjectSaving : AppState -> Model -> Html Msg
viewProjectNavigationProjectSaving appState model =
    Html.map ProjectSavingMsg <|
        ProjectSaving.view appState model.projectSavingModel


viewProjectNavigationActions : AppState -> Model -> QuestionnaireCommon -> Html Msg
viewProjectNavigationActions appState model questionnaire =
    if QuestionnaireUtils.isAnonymousProject questionnaire && Session.exists appState.session then
        DetailNavigation.sectionActions
            [ ActionResultView.error model.addingToMyProjects
            , ActionButton.buttonCustom
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
            [ viewProjectNavigationShareButton appState model questionnaire ]

    else
        Html.nothing


viewProjectNavigationShareButton : AppState -> Model -> QuestionnaireCommon -> Html Msg
viewProjectNavigationShareButton appState model questionnaire =
    div [ class "btn-group", dataTour "project_detail_share" ]
        [ button
            [ class "btn btn-info text-light with-icon"
            , onClick (ShareModalMsg <| ShareModal.openMsg questionnaire)
            , dataCy "project_detail_share-button"
            ]
            [ shareIcon questionnaire
            , text (gettext "Share" appState.locale)
            ]
        , Dropdown.dropdown model.shareDropdownState
            { options =
                [ Dropdown.attrs [ class "ShareDropdown" ]
                , Dropdown.alignMenuRight
                ]
            , toggleMsg = ShareDropdownMsg
            , toggleButton = Dropdown.toggle [ Button.info ] []
            , items =
                [ Dropdown.anchorItem [ onClick ShareDropdownCopyLink ]
                    [ faQuestionnaireCopyLink
                    , text (gettext "Copy link" appState.locale)
                    ]
                , Dropdown.divider
                , Dropdown.customItem <|
                    div [ class "px-3 py-2 text-muted" ] (shareTooltipHtml appState questionnaire)
                ]
            }
        ]



-- PROJECT - NAVIGATION - NAV ROW


viewProjectNavigationNav : AppState -> ProjectDetailRoute -> Model -> QuestionnaireCommon -> Html Msg
viewProjectNavigationNav appState route model questionnaire =
    let
        projectRoute subroute =
            Wizard.Routes.ProjectsRoute (PlansRoutes.DetailRoute model.uuid subroute)

        isQuestionnaireRoute =
            case route of
                ProjectDetailRoute.Questionnaire _ _ ->
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

        isFilesRoute =
            case route of
                ProjectDetailRoute.Files _ ->
                    True

                _ ->
                    False

        questionnaireLink =
            { route = projectRoute (ProjectDetailRoute.Questionnaire Nothing Nothing)
            , label = gettext "Questionnaire" appState.locale
            , icon = faProjectQuestionnaire
            , isActive = isQuestionnaireRoute
            , isVisible = True
            , dataCy = "project_nav_questionnaire"
            }

        metricsLink =
            { route = projectRoute ProjectDetailRoute.Metrics
            , label = gettext "Metrics" appState.locale
            , icon = faProjectMetrics
            , isActive = route == ProjectDetailRoute.Metrics
            , isVisible = Features.projectMetrics appState
            , dataCy = "project_nav_metrics"
            }

        previewLink =
            { route = projectRoute ProjectDetailRoute.Preview
            , label = gettext "Preview" appState.locale
            , icon = faPreview
            , isActive = route == ProjectDetailRoute.Preview
            , isVisible = Features.projectPreview appState
            , dataCy = "project_nav_preview"
            }

        documentsLink =
            { route = projectRoute (ProjectDetailRoute.Documents PaginationQueryString.empty)
            , label = gettext "Documents" appState.locale
            , icon = faProjectDocuments
            , isActive = isDocumentRoute
            , isVisible = Features.projectDocumentsView appState
            , dataCy = "project_nav_documents"
            }

        questionnaireFiles =
            model.questionnaireModel
                |> ActionResult.unwrap 0 (List.length << .files << .questionnaire)

        filesVisible =
            questionnaire.fileCount > 0 || questionnaireFiles > 0

        filesLink =
            { route = projectRoute (ProjectDetailRoute.Files PaginationQueryString.empty)
            , label = gettext "Files" appState.locale
            , icon = faProjectFiles
            , isActive = isFilesRoute
            , isVisible = filesVisible
            , dataCy = "project_nav_files"
            }

        settingsLink =
            { route = projectRoute ProjectDetailRoute.Settings
            , label = gettext "Settings" appState.locale
            , icon = faSettings
            , isActive = route == ProjectDetailRoute.Settings
            , isVisible = Features.projectSettings appState questionnaire
            , dataCy = "project_nav_settings"
            }

        links =
            [ questionnaireLink
            , metricsLink
            , previewLink
            , documentsLink
            , filesLink
            , settingsLink
            ]
    in
    DetailNavigation.navigation links



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
        ProjectDetailRoute.Questionnaire _ _ ->
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
                        , renderer = DefaultQuestionnaireRenderer.create appState qm.questionnaire.knowledgeModel (DefaultQuestionnaireRenderer.defaultResourcePageToRoute qm.questionnaire.packageId)
                        , wrapMsg = QuestionnaireMsg
                        , previewQuestionnaireEventMsg = Just (OpenVersionPreview qm.questionnaire.uuid)
                        , revertQuestionnaireMsg = Just OpenRevertModal
                        , isKmEditor = False
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

        ProjectDetailRoute.Files _ ->
            Html.map FilesMsg <| Files.view appState questionnaire model.filesModel

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
