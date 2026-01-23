module Wizard.Components.Questionnaire2 exposing
    ( Model
    , Msg
    , UpdateConfig
    , ViewConfig
    , addFile
    , applyProjectEvent
    , dispatchScrollToQuestion
    , init
    , initSimple
    , openChapterMsg
    , resetUserSuggestionDropdownModels
    , setPhaseMsg
    , subscriptions
    , update
    , updateContentScrollTopMsg
    , updateWithQuestionnaireData
    , view
    , virtualizeContent
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError exposing (ApiError)
import Common.Ports.Dom as Dom
import Common.Ports.Dom.ElementScrollTop as ElementScrollTop
import Common.Ports.LocalStorage as LocalStorage
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Common.Utils.ShortcutUtils as Shortcut
import Compose exposing (compose2)
import Dict exposing (Dict)
import Gettext exposing (gettext)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)
import Html.Attributes.Extensions exposing (dataTour)
import Html.Extra as Html
import Json.Decode as D
import Json.Encode as E
import List.Extra as List
import Maybe.Extra as Maybe
import Set exposing (Set)
import Shortcut
import SplitPane
import Task.Extra as Task
import Uuid exposing (Uuid)
import Uuid.Extra as Uuid
import Wizard.Api.Models.BootstrapConfig.UserConfig as UserConfig
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Api.Models.Project.ProjectTodo exposing (ProjectTodo)
import Wizard.Api.Models.ProjectCommon exposing (ProjectCommon)
import Wizard.Api.Models.ProjectDetail.Comment exposing (Comment)
import Wizard.Api.Models.ProjectDetail.CommentThread exposing (CommentThread)
import Wizard.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.AddCommentData as AddCommentData exposing (AddCommentData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.AssignCommentThreadData exposing (AssignCommentThreadData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.DeleteCommentData exposing (DeleteCommentData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.DeleteCommentThreadData exposing (DeleteCommentThreadData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.EditCommentData exposing (EditCommentData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.ReopenCommentThreadData exposing (ReopenCommentThreadData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.ResolveCommentThreadData exposing (ResolveCommentThreadData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.SetReplyData as SetReplyData
import Wizard.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue
import Wizard.Api.Models.ProjectFileSimple exposing (ProjectFileSimple)
import Wizard.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire, QuestionnaireWarning)
import Wizard.Api.Models.WebSockets.ProjectMessage.SetProjectData exposing (SetProjectData)
import Wizard.Api.Projects as ProjectsApi
import Wizard.Components.PluginModal as PluginModal
import Wizard.Components.Questionnaire2.Components.CommentsRightPanel as CommentsRightPanel
import Wizard.Components.Questionnaire2.Components.FileDeleteModal as FileDeleteModal
import Wizard.Components.Questionnaire2.Components.FileUploadModal as FileUploadModal exposing (FileConfig)
import Wizard.Components.Questionnaire2.Components.NavigationTree as NavigationTree
import Wizard.Components.Questionnaire2.Components.PhaseSelection as PhaseSelection
import Wizard.Components.Questionnaire2.Components.QuestionnaireContent as QuestionnaireContent
import Wizard.Components.Questionnaire2.Components.SearchRightPanel as SearchRightPanel
import Wizard.Components.Questionnaire2.Components.TodosRightPanel as TodosRightPanel
import Wizard.Components.Questionnaire2.Components.Toolbar as Toolbar
import Wizard.Components.Questionnaire2.Components.VersionHistoryRightPanel as VersionHistoryRightPanel
import Wizard.Components.Questionnaire2.Components.WarningsRightPanel as WarningsRightPanel
import Wizard.Components.Questionnaire2.QuestionnaireLocalStorage as QuestionnaireLocalStorage
import Wizard.Components.Questionnaire2.QuestionnaireRightPanel as QuestionnaireRightPanel exposing (PluginQuestionActionData, QuestionnaireRightPanel)
import Wizard.Components.Questionnaire2.QuestionnaireUpdateReturnData as QuestionnaireUpdateReturnData exposing (QuestionnaireUpdateReturnData)
import Wizard.Components.Questionnaire2.QuestionnaireViewSettings as QuestionnaireViewSettings exposing (QuestionnaireViewSettings)
import Wizard.Components.Questionnaire2.QuestionnaireVirtualization as QuestionnaireVirtualization exposing (ContentNode)
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Plugins.Plugin as Plugin
import Wizard.Plugins.PluginElement as PluginElement exposing (PluginElement)
import Wizard.Routes
import Wizard.Utils.Feature as Feature


type alias Model =
    { uuid : Uuid
    , contentSplitPane : SplitPane.State
    , rightPanelSplitPane : SplitPane.State
    , rightPanelClosedSplitPane : SplitPane.State
    , rightPanel : QuestionnaireRightPanel
    , contentScrollTop : Maybe Int

    -- data
    , chapterUnansweredQuestions : Dict String Int
    , chapterUuid : String
    , collapsedPaths : Set String
    , commentThreadsMap : Dict String (ActionResult (List CommentThread))
    , content : List ContentNode
    , knowledgeModelParentMap : KnowledgeModel.ParentMap
    , mbCommentThreadUuid : Maybe Uuid
    , mbHighlightedPath : Maybe String
    , questionnaire : ProjectQuestionnaire
    , todos : List ProjectTodo
    , viewNamedOnlyVersions : Bool
    , viewResolvedComments : Bool
    , viewSettings : QuestionnaireViewSettings
    , warnings : List QuestionnaireWarning

    -- component models
    , commentsRightPanelModel : CommentsRightPanel.Model
    , contentModel : QuestionnaireContent.Model
    , fileDeleteModalModel : FileDeleteModal.Model
    , fileUploadModalModel : FileUploadModal.Model
    , phaseSelectionModel : PhaseSelection.Model
    , pluginProjectActionModalModel : PluginModal.Model ProjectCommon
    , pluginProjectQuestionActionModalModel : PluginModal.Model ( ProjectCommon, Question, String )
    , searchRightPanelModel : SearchRightPanel.Model
    , toolbarModel : Toolbar.Model
    , versionHistoryRightPanelModel : VersionHistoryRightPanel.Model
    }


initSimple : AppState -> ProjectQuestionnaire -> ( Model, Cmd Msg )
initSimple appState questionnaire =
    init appState questionnaire Nothing Nothing


init : AppState -> ProjectQuestionnaire -> Maybe String -> Maybe Uuid -> ( Model, Cmd Msg )
init appState projectQuestionnaire mbPath mbCommentThreadUuid =
    let
        chapterUuid =
            List.head projectQuestionnaire.knowledgeModel.chapterUuids
                |> Maybe.withDefault ""

        scrollCmd =
            case mbPath of
                Just path ->
                    Task.dispatch <| ScrollToPath False path

                Nothing ->
                    Cmd.none

        plugins =
            AppState.getPluginsByConnector appState .projectQuestionActions
                |> Plugin.filterByKmPatterns (KnowledgeModelUtils.getPackageId projectQuestionnaire.knowledgeModelPackage)
                |> List.sortBy (.name << .action << Tuple.second)

        initialModel =
            { uuid = projectQuestionnaire.uuid
            , contentSplitPane = SplitPane.init SplitPane.Horizontal |> SplitPane.configureSplitter (SplitPane.percentage 0.2 (Just ( 0.1, 0.7 )))
            , rightPanelSplitPane = SplitPane.init SplitPane.Horizontal |> SplitPane.configureSplitter (SplitPane.percentage 0.75 (Just ( 0.3, 0.9 )))
            , rightPanelClosedSplitPane = SplitPane.init SplitPane.Horizontal |> SplitPane.configureSplitter (SplitPane.percentage 1 (Just ( 1, 0 )))
            , rightPanel = QuestionnaireRightPanel.None
            , contentScrollTop = Nothing

            -- data
            , chapterUnansweredQuestions = Dict.empty
            , chapterUuid = chapterUuid
            , collapsedPaths = Set.empty
            , commentThreadsMap = Dict.empty
            , content = []
            , knowledgeModelParentMap = KnowledgeModel.createParentMap projectQuestionnaire.knowledgeModel
            , mbCommentThreadUuid = mbCommentThreadUuid
            , mbHighlightedPath = Nothing
            , questionnaire = projectQuestionnaire
            , todos = ProjectQuestionnaire.getTodos projectQuestionnaire
            , viewNamedOnlyVersions = False
            , viewResolvedComments = False
            , viewSettings = QuestionnaireViewSettings.default
            , warnings = ProjectQuestionnaire.getWarnings projectQuestionnaire

            -- component models
            , commentsRightPanelModel = CommentsRightPanel.init projectQuestionnaire.uuid
            , contentModel = QuestionnaireContent.init plugins
            , fileDeleteModalModel = FileDeleteModal.init projectQuestionnaire.uuid
            , fileUploadModalModel = FileUploadModal.init projectQuestionnaire.uuid
            , phaseSelectionModel = PhaseSelection.init
            , pluginProjectActionModalModel = PluginModal.initialModel
            , pluginProjectQuestionActionModalModel = PluginModal.initialModel
            , searchRightPanelModel = SearchRightPanel.init
            , toolbarModel = Toolbar.init appState projectQuestionnaire
            , versionHistoryRightPanelModel = VersionHistoryRightPanel.init appState projectQuestionnaire.uuid
            }
    in
    ( initialModel
        |> virtualizeContent
        |> calculateUnansweredQuestions
    , Cmd.batch
        [ QuestionnaireLocalStorage.getItems projectQuestionnaire.uuid
        , Task.dispatch UpdateContentScrollTop
        , scrollCmd
        ]
    )


addFile : ProjectFileSimple -> Model -> Model
addFile file model =
    { model | questionnaire = ProjectQuestionnaire.addFile file model.questionnaire }


updateWithQuestionnaireData : AppState -> SetProjectData -> Model -> Model
updateWithQuestionnaireData appState data model =
    let
        updatedQuestionnaire =
            ProjectQuestionnaire.updateWithQuestionnaireData data model.questionnaire

        setNewPanel panel allowed =
            if allowed then
                panel

            else
                QuestionnaireRightPanel.None

        rightPanel =
            case model.rightPanel of
                QuestionnaireRightPanel.TODOs ->
                    setNewPanel QuestionnaireRightPanel.TODOs <|
                        Feature.projectTodos appState updatedQuestionnaire

                QuestionnaireRightPanel.VersionHistory ->
                    setNewPanel QuestionnaireRightPanel.VersionHistory <|
                        Feature.projectVersionHistory appState updatedQuestionnaire

                QuestionnaireRightPanel.CommentsOverview ->
                    setNewPanel QuestionnaireRightPanel.CommentsOverview <|
                        Feature.projectCommentAdd appState updatedQuestionnaire

                QuestionnaireRightPanel.Comments path ->
                    setNewPanel (QuestionnaireRightPanel.Comments path) <|
                        Feature.projectCommentAdd appState updatedQuestionnaire

                _ ->
                    model.rightPanel
    in
    { model
        | questionnaire = updatedQuestionnaire
        , rightPanel = rightPanel
    }


resetUserSuggestionDropdownModels : Model -> Model
resetUserSuggestionDropdownModels model =
    { model | commentsRightPanelModel = CommentsRightPanel.resetUserSuggestionDropdownModels model.commentsRightPanelModel }


type Msg
    = ContentSplitPaneMsg SplitPane.Msg
    | RightPanelSplitPaneMsg SplitPane.Msg
    | LocalStorageGotData E.Value
    | QuestionnaireContentMsg QuestionnaireContent.Msg
    | PhaseSelectionMsg PhaseSelection.Msg
    | NavigationTreeMsg NavigationTree.Msg
    | ToolbarMsg Toolbar.Msg
    | SearchRightPanelMsg SearchRightPanel.Msg
    | TodosRightPanelMsg TodosRightPanel.Msg
    | WarningsRightPanelMsg WarningsRightPanel.Msg
    | CommentsRightPanelMsg CommentsRightPanel.Msg
    | VersionHistoryRightPanelMsg VersionHistoryRightPanel.Msg
    | OpenChapter Bool String
    | OpenComments String
    | UpdateCollapsedPaths (Set String)
    | UpdateViewSettings QuestionnaireViewSettings
    | UpdateRightPanel QuestionnaireRightPanel
    | UpdateViewResolvedComments Bool
    | UpdateViewNamedOnlyVersions Bool
    | ScrollToPath Bool String
    | ScrollToQuestion String
    | ClearHighlightedPath String
    | SetPhase (Maybe Uuid)
    | SetLabels String (List String)
    | FileUploadModalMsg FileUploadModal.Msg
    | FileDeleteModalMsg FileDeleteModal.Msg
    | SetFile String ProjectFileSimple
    | RemoveFile String
    | PluginProjectActionModalMsg (PluginModal.Msg ProjectCommon)
    | PluginProjectQuestionActionModalMsg (PluginModal.Msg ( ProjectCommon, Question, String ))
    | SetFullScreen Bool
    | GetCommentThreadsCompleted String (Result ApiError (Dict String (List CommentThread)))
    | UpdateContentScrollTop
    | GotContentScrollTop E.Value


openChapterMsg : String -> Msg
openChapterMsg =
    OpenChapter False


setPhaseMsg : Maybe Uuid -> Msg
setPhaseMsg =
    SetPhase


updateContentScrollTopMsg : Msg
updateContentScrollTopMsg =
    UpdateContentScrollTop


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , mbSetFullScreenMsg : Maybe (Bool -> msg)
    , projectCommon : ProjectCommon
    }


update : AppState -> UpdateConfig msg -> Msg -> Model -> QuestionnaireUpdateReturnData Model msg
update appState cfg msg model =
    let
        tryApplyProjectEvent mbEvent m =
            case mbEvent of
                Just event ->
                    applyProjectEvent event m

                Nothing ->
                    m

        handleScrollToPath instant path =
            let
                pathParts =
                    String.split "." path

                chapterUuid =
                    List.head pathParts |> Maybe.withDefault ""

                createSubpaths parts =
                    case parts of
                        [] ->
                            []

                        _ ->
                            let
                                rest =
                                    List.unconsLast parts
                                        |> Maybe.unwrap [] Tuple.second
                            in
                            String.join "." parts :: createSubpaths rest

                newCollapsedPaths =
                    createSubpaths pathParts
                        |> List.foldl (\currentPath collapsedItems -> Set.remove currentPath collapsedItems) model.collapsedPaths

                selector =
                    "[data-path=\"" ++ path ++ "\"]"

                scrollIntoViewCmd =
                    if instant then
                        Dom.scrollIntoViewInstant selector

                    else
                        Dom.scrollIntoView selector

                questionnaireContentModel =
                    if chapterUuid /= model.chapterUuid || newCollapsedPaths /= model.collapsedPaths then
                        virtualizeContent
                            { model
                                | chapterUuid = chapterUuid
                                , collapsedPaths = newCollapsedPaths
                            }

                    else
                        model

                newModel =
                    { questionnaireContentModel | mbHighlightedPath = Just path }

                cmds =
                    Cmd.batch
                        [ scrollIntoViewCmd
                        , QuestionnaireLocalStorage.updateCollapsedPaths model.questionnaire.uuid newCollapsedPaths
                        , Task.dispatchAfter 3000 (cfg.wrapMsg (ClearHighlightedPath path))
                        ]
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState newModel cmds
    in
    case msg of
        ContentSplitPaneMsg splitPaneMsg ->
            QuestionnaireUpdateReturnData.fromModel appState
                { model | contentSplitPane = SplitPane.update splitPaneMsg model.contentSplitPane }

        RightPanelSplitPaneMsg splitPaneMsg ->
            QuestionnaireUpdateReturnData.fromModel appState
                { model | rightPanelSplitPane = SplitPane.update splitPaneMsg model.rightPanelSplitPane }

        LocalStorageGotData json ->
            let
                localStorageCmd =
                    QuestionnaireLocalStorage.applyLocalStorageData
                        model.questionnaire.uuid
                        json
                        { updateViewSettings = dispatchUpdateViewSettings cfg.wrapMsg
                        , updateCollapsedItems = dispatchUpdateCollapsedPaths cfg.wrapMsg
                        , updateRightPanel = dispatchUpdateRightPanel cfg.wrapMsg
                        , updateViewResolved = dispatchUpdateViewResolvedComments cfg.wrapMsg
                        , updateNamedOnly = dispatchUpdateViewNamedOnlyVersions cfg.wrapMsg
                        }
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState model localStorageCmd

        QuestionnaireContentMsg contentMsg ->
            let
                contentUpdateConfig =
                    { addTodoCmd = dispatchSetLabels cfg.wrapMsg [ ProjectQuestionnaire.todoUuid ]
                    , chapterUuid = model.chapterUuid
                    , closeRightPanelCmd = dispatchUpdateRightPanel cfg.wrapMsg QuestionnaireRightPanel.None
                    , collapsedPaths = model.collapsedPaths
                    , deleteFileCmd = dispatchDeleteFile cfg.wrapMsg
                    , knowledgeModelParentMap = model.knowledgeModelParentMap
                    , mbKmEditorUuid = Nothing
                    , openChapterCmd = dispatchOpenChapter cfg.wrapMsg
                    , openCommentsCmd = dispatchUpdateRightPanel cfg.wrapMsg << QuestionnaireRightPanel.Comments
                    , openFileUploadCmd = dispatchOpenFileUploadModal cfg.wrapMsg
                    , openPluginQuestionActionModalCmd = dispatchOpenPluginQuestionActionModal cfg.wrapMsg cfg.projectCommon
                    , openPluginQuestionActionRightPanelCmd = dispatchOpenPluginQuestionActionRightPanel cfg.wrapMsg
                    , questionnaire = model.questionnaire
                    , removeTodoCmd = dispatchSetLabels cfg.wrapMsg []
                    , scrollToPathCmd = dispatchScrollToPath cfg.wrapMsg
                    , updateCollapsedPathsCmd = dispatchUpdateCollapsedPaths cfg.wrapMsg
                    , wrapMsg = cfg.wrapMsg << QuestionnaireContentMsg
                    }

                updateReturnData =
                    QuestionnaireContent.update appState contentUpdateConfig contentMsg model.contentModel

                newModel =
                    tryApplyProjectEvent updateReturnData.event
                        { model | contentModel = updateReturnData.model }
            in
            { seed = updateReturnData.seed
            , model = newModel
            , cmd = updateReturnData.cmd
            , event = updateReturnData.event
            }

        PhaseSelectionMsg phaseSelectionMsg ->
            let
                ( newPhaseSelectionModel, phaseSelectionCmd ) =
                    PhaseSelection.update
                        { setPhaseCmd = Task.dispatch << cfg.wrapMsg << SetPhase
                        }
                        phaseSelectionMsg
                        model.phaseSelectionModel

                newModel =
                    { model | phaseSelectionModel = newPhaseSelectionModel }
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState newModel phaseSelectionCmd

        NavigationTreeMsg navigationTreeMsg ->
            let
                cmd =
                    NavigationTree.update
                        { openChapterCmd = dispatchOpenChapter cfg.wrapMsg
                        , scrollToPathCmd = dispatchScrollToPath cfg.wrapMsg False
                        , updateCollapsedPathsCmd = dispatchUpdateCollapsedPaths cfg.wrapMsg
                        }
                        navigationTreeMsg
            in
            { seed = appState.seed
            , model = model
            , cmd = cmd
            , event = Nothing
            }

        ToolbarMsg toolbarMsg ->
            let
                ( newToolbarModel, toolbarCmd ) =
                    Toolbar.update
                        { updateViewSettingsCmd = dispatchUpdateViewSettings cfg.wrapMsg
                        , openProjectActionModalCmd = dispatchOpenProjectActionModal cfg.wrapMsg cfg.projectCommon
                        , updateRightPanelCmd = dispatchUpdateRightPanel cfg.wrapMsg
                        , setFullScreenCmd = dispatchSetFullScreen cfg.wrapMsg
                        }
                        toolbarMsg
                        model.toolbarModel

                newModel =
                    { model | toolbarModel = newToolbarModel }
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState newModel toolbarCmd

        SearchRightPanelMsg searchMsg ->
            let
                ( newSearchRightPanelModel, searchRightPanelCmd ) =
                    SearchRightPanel.update appState model.questionnaire searchMsg model.searchRightPanelModel

                newModel =
                    { model | searchRightPanelModel = newSearchRightPanelModel }
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState
                newModel
                (Cmd.map (cfg.wrapMsg << SearchRightPanelMsg) searchRightPanelCmd)

        TodosRightPanelMsg todosMsg ->
            let
                cmd =
                    TodosRightPanel.update
                        { scrollToPathCmd = dispatchScrollToPath cfg.wrapMsg False }
                        todosMsg
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState model cmd

        WarningsRightPanelMsg warningsMsg ->
            let
                cmd =
                    WarningsRightPanel.update
                        { scrollToPathCmd = dispatchScrollToPath cfg.wrapMsg False }
                        warningsMsg
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState model cmd

        CommentsRightPanelMsg commentsMsg ->
            let
                commentsRightPanelReturnData =
                    CommentsRightPanel.update appState commentsMsg model.commentsRightPanelModel

                newModel =
                    case commentsRightPanelReturnData.event of
                        Just event ->
                            applyProjectEvent event model

                        Nothing ->
                            model
            in
            { seed = commentsRightPanelReturnData.seed
            , model = { newModel | commentsRightPanelModel = commentsRightPanelReturnData.model }
            , cmd = Cmd.map (cfg.wrapMsg << CommentsRightPanelMsg) commentsRightPanelReturnData.cmd
            , event = commentsRightPanelReturnData.event
            }

        VersionHistoryRightPanelMsg versionHistoryMsg ->
            let
                ( newVersionHistoryRightPanelModel, versionHistoryCmd ) =
                    VersionHistoryRightPanel.update appState versionHistoryMsg model.versionHistoryRightPanelModel

                newModel =
                    { model | versionHistoryRightPanelModel = newVersionHistoryRightPanelModel }
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState
                newModel
                (Cmd.map (cfg.wrapMsg << VersionHistoryRightPanelMsg) versionHistoryCmd)

        OpenChapter highlight chapterUuid ->
            let
                ( highlightedPathModel, highlightedCmd ) =
                    if highlight then
                        ( { model | mbHighlightedPath = Just chapterUuid }
                        , Task.dispatchAfter 3000 (cfg.wrapMsg (ClearHighlightedPath chapterUuid))
                        )

                    else
                        ( model, Cmd.none )

                newModel =
                    virtualizeContent { highlightedPathModel | chapterUuid = chapterUuid }
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState
                newModel
                (Cmd.batch
                    [ Dom.scrollToTop contentElementSelector
                    , highlightedCmd
                    ]
                )

        OpenComments path ->
            QuestionnaireUpdateReturnData.fromModelCmd appState
                model
                (Cmd.batch
                    [ Task.dispatch (cfg.wrapMsg (UpdateRightPanel (QuestionnaireRightPanel.Comments path)))
                    , Task.dispatch (cfg.wrapMsg (ScrollToPath False path))
                    ]
                )

        UpdateCollapsedPaths paths ->
            let
                localStorageCmd =
                    QuestionnaireLocalStorage.updateCollapsedPaths model.questionnaire.uuid paths

                newModel =
                    virtualizeContent { model | collapsedPaths = paths }
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState newModel localStorageCmd

        UpdateViewResolvedComments viewResolved ->
            let
                localStorageCmd =
                    QuestionnaireLocalStorage.updateViewResolved model.questionnaire.uuid viewResolved

                newModel =
                    { model | viewResolvedComments = viewResolved }
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState newModel localStorageCmd

        UpdateViewNamedOnlyVersions viewNamedOnlyVersions ->
            let
                localStorageCmd =
                    QuestionnaireLocalStorage.updateNamedOnly model.questionnaire.uuid viewNamedOnlyVersions

                newModel =
                    { model | viewNamedOnlyVersions = viewNamedOnlyVersions }
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState newModel localStorageCmd

        UpdateViewSettings viewSettings ->
            let
                localStorageCmd =
                    QuestionnaireLocalStorage.updateViewSettings viewSettings

                newModel =
                    virtualizeContent { model | viewSettings = viewSettings }
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState newModel localStorageCmd

        UpdateRightPanel rightPanel ->
            let
                localStorageCmd =
                    QuestionnaireLocalStorage.updateRightPanel model.questionnaire.uuid rightPanel

                newContent =
                    case rightPanel of
                        QuestionnaireRightPanel.PluginQuestionAction pluginQuestionActionData ->
                            QuestionnaireVirtualization.setPluginOpen
                                (Question.getUuid pluginQuestionActionData.question)
                                ( pluginQuestionActionData.plugin.uuid, pluginQuestionActionData.connector )
                                model.content

                        _ ->
                            case model.rightPanel of
                                QuestionnaireRightPanel.PluginQuestionAction _ ->
                                    QuestionnaireVirtualization.clearPluginOpen model.content

                                _ ->
                                    model.content

                newModel =
                    { model
                        | content = newContent
                        , rightPanel = rightPanel
                    }

                panelCmd =
                    case rightPanel of
                        QuestionnaireRightPanel.Search ->
                            Dom.focus ("#" ++ SearchRightPanel.searchInputId)

                        QuestionnaireRightPanel.Comments path ->
                            ProjectsApi.getCommentThreads appState model.uuid path (cfg.wrapMsg << GetCommentThreadsCompleted path)

                        QuestionnaireRightPanel.VersionHistory ->
                            Task.dispatch (cfg.wrapMsg <| VersionHistoryRightPanelMsg VersionHistoryRightPanel.initMsg)

                        _ ->
                            Cmd.none

                cmds =
                    Cmd.batch
                        [ localStorageCmd
                        , panelCmd
                        ]
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState newModel cmds

        ScrollToPath instant path ->
            handleScrollToPath instant path

        ScrollToQuestion questionUuid ->
            case ProjectQuestionnaire.getClosestQuestionParentPath model.questionnaire model.knowledgeModelParentMap questionUuid of
                Just path ->
                    handleScrollToPath True path

                Nothing ->
                    QuestionnaireUpdateReturnData.fromModel appState model

        ClearHighlightedPath path ->
            if model.mbHighlightedPath == Just path then
                QuestionnaireUpdateReturnData.fromModel appState { model | mbHighlightedPath = Nothing }

            else
                QuestionnaireUpdateReturnData.fromModel appState model

        SetPhase mbPhaseUuid ->
            let
                ( newUuid, newSeed ) =
                    Uuid.step appState.seed

                setPhaseEvent =
                    ProjectEvent.SetPhase
                        { uuid = newUuid
                        , phaseUuid = mbPhaseUuid
                        , createdAt = appState.currentTime
                        , createdBy = Maybe.map UserConfig.toUserSuggestion appState.config.user
                        }
            in
            { seed = newSeed
            , model = virtualizeContent (applyProjectEvent setPhaseEvent model)
            , cmd = Cmd.none
            , event = Just setPhaseEvent
            }

        SetLabels path labels ->
            let
                ( newUuid, newSeed ) =
                    Uuid.step appState.seed

                setLabelsEvent =
                    ProjectEvent.SetLabels
                        { uuid = newUuid
                        , path = path
                        , value = labels
                        , createdAt = appState.currentTime
                        , createdBy = Maybe.map UserConfig.toUserSuggestion appState.config.user
                        }
            in
            { seed = newSeed
            , model = applyProjectEvent setLabelsEvent model
            , cmd = Cmd.none
            , event = Just setLabelsEvent
            }

        FileUploadModalMsg fileUploadModalMsg ->
            let
                updateConfig =
                    { wrapMsg = cfg.wrapMsg << FileUploadModalMsg
                    , setFileMsg = compose2 cfg.wrapMsg SetFile
                    }

                ( fileUploadModalModel, fileUploadModalCmd ) =
                    FileUploadModal.update appState updateConfig fileUploadModalMsg model.fileUploadModalModel
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState
                { model | fileUploadModalModel = fileUploadModalModel }
                fileUploadModalCmd

        FileDeleteModalMsg fileDeleteModalMsg ->
            let
                updateConfig =
                    { wrapMsg = cfg.wrapMsg << FileDeleteModalMsg
                    , deleteFileMsg = cfg.wrapMsg << RemoveFile
                    }

                ( fileDeleteModalModel, fileDeleteModalCmd ) =
                    FileDeleteModal.update appState updateConfig fileDeleteModalMsg model.fileDeleteModalModel
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState
                { model | fileDeleteModalModel = fileDeleteModalModel }
                fileDeleteModalCmd

        SetFile path file ->
            let
                ( newUuid, newSeed ) =
                    Uuid.step appState.seed

                setReplyEvent =
                    ProjectEvent.SetReply
                        { uuid = newUuid
                        , path = path
                        , value = ReplyValue.FileReply file.uuid
                        , createdAt = appState.currentTime
                        , createdBy = Maybe.map UserConfig.toUserSuggestion appState.config.user
                        }

                newModel =
                    { model | questionnaire = ProjectQuestionnaire.addFile file model.questionnaire }
            in
            { seed = newSeed
            , model = applyProjectEvent setReplyEvent newModel
            , cmd = Cmd.none
            , event = Just setReplyEvent
            }

        RemoveFile path ->
            let
                ( newUuid, newSeed ) =
                    Uuid.step appState.seed

                clearReplyEvent =
                    ProjectEvent.ClearReply
                        { uuid = newUuid
                        , path = path
                        , createdAt = appState.currentTime
                        , createdBy = Maybe.map UserConfig.toUserSuggestion appState.config.user
                        }
            in
            { seed = newSeed
            , model = applyProjectEvent clearReplyEvent model
            , cmd = Cmd.none
            , event = Just clearReplyEvent
            }

        PluginProjectActionModalMsg pluginModalMsg ->
            let
                pluginProjectActionModalModel =
                    PluginModal.update pluginModalMsg model.pluginProjectActionModalModel
            in
            QuestionnaireUpdateReturnData.fromModel appState
                { model | pluginProjectActionModalModel = pluginProjectActionModalModel }

        PluginProjectQuestionActionModalMsg pluginModalMsg ->
            let
                pluginProjectQuestionActionModalModel =
                    PluginModal.update pluginModalMsg model.pluginProjectQuestionActionModalModel
            in
            QuestionnaireUpdateReturnData.fromModel appState
                { model | pluginProjectQuestionActionModalModel = pluginProjectQuestionActionModalModel }

        SetFullScreen isFullScreen ->
            case cfg.mbSetFullScreenMsg of
                Just setFullScreenMsg ->
                    QuestionnaireUpdateReturnData.fromModelCmd appState model (Task.dispatch (setFullScreenMsg isFullScreen))

                Nothing ->
                    QuestionnaireUpdateReturnData.fromModel appState model

        GetCommentThreadsCompleted path result ->
            case result of
                Ok threads ->
                    let
                        newModel =
                            case Dict.get path threads of
                                Just commentThreads ->
                                    { model | commentThreadsMap = Dict.insert path (Success commentThreads) model.commentThreadsMap }

                                Nothing ->
                                    { model | commentThreadsMap = Dict.insert path (Success []) model.commentThreadsMap }
                    in
                    case model.mbCommentThreadUuid of
                        Just threadUuid ->
                            let
                                selector =
                                    "[data-comment-thread-uuid=\"" ++ Uuid.toString threadUuid ++ "\"]"

                                ( isPrivate, isResolved ) =
                                    Dict.get path newModel.commentThreadsMap
                                        |> Maybe.andThen ActionResult.toMaybe
                                        |> Maybe.andThen (List.find (\t -> t.uuid == threadUuid))
                                        |> Maybe.unwrap ( False, False ) (\t -> ( t.private, t.resolved ))
                            in
                            QuestionnaireUpdateReturnData.fromModelCmd appState
                                { newModel
                                    | mbCommentThreadUuid = Nothing
                                    , viewResolvedComments = isResolved
                                    , commentsRightPanelModel = CommentsRightPanel.setViewPrivateAndResolved isPrivate isResolved model.commentsRightPanelModel
                                }
                                (Dom.scrollIntoView selector)

                        Nothing ->
                            QuestionnaireUpdateReturnData.fromModel appState newModel

                Err _ ->
                    QuestionnaireUpdateReturnData.fromModel appState
                        { model
                            | commentThreadsMap = Dict.insert path (Error (gettext "Unable to get comments." appState.locale)) model.commentThreadsMap
                        }

        UpdateContentScrollTop ->
            let
                subscribeCmd =
                    Dom.subscribeScrollTop contentElementSelector

                cmds =
                    case model.contentScrollTop of
                        Just value ->
                            Cmd.batch
                                [ subscribeCmd
                                , Dom.setScrollTop
                                    { selector = contentElementSelector
                                    , scrollTop = value
                                    }
                                ]

                        Nothing ->
                            subscribeCmd
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState model cmds

        GotContentScrollTop value ->
            let
                newModel =
                    case D.decodeValue ElementScrollTop.decoder value of
                        Ok elementScrollTop ->
                            if elementScrollTop.selector == contentElementSelector then
                                { model | contentScrollTop = Just elementScrollTop.scrollTop }

                            else
                                model

                        Err _ ->
                            model
            in
            QuestionnaireUpdateReturnData.fromModel appState newModel


dispatchDeleteFile : (Msg -> msg) -> Uuid -> String -> String -> Cmd msg
dispatchDeleteFile wrapMsg fileUuid fileName questionPath =
    Task.dispatch <| wrapMsg <| FileDeleteModalMsg <| FileDeleteModal.open fileUuid fileName questionPath


dispatchOpenFileUploadModal : (Msg -> msg) -> String -> FileConfig -> Cmd msg
dispatchOpenFileUploadModal wrapMsg questionPath fileConfig =
    Task.dispatch <| wrapMsg <| FileUploadModalMsg <| FileUploadModal.open questionPath fileConfig


dispatchOpenChapter : (Msg -> msg) -> String -> Cmd msg
dispatchOpenChapter wrapMsg chapterUuid =
    Task.dispatch <| wrapMsg <| OpenChapter False chapterUuid


dispatchScrollToPath : (Msg -> msg) -> Bool -> String -> Cmd msg
dispatchScrollToPath wrapMsg instant path =
    Task.dispatch <| wrapMsg <| ScrollToPath instant path


dispatchScrollToQuestion : (Msg -> msg) -> String -> Cmd msg
dispatchScrollToQuestion wrapMsg questionUuid =
    Task.dispatch <| wrapMsg <| ScrollToQuestion questionUuid


dispatchUpdateCollapsedPaths : (Msg -> msg) -> Set String -> Cmd msg
dispatchUpdateCollapsedPaths wrapMsg =
    Task.dispatch << wrapMsg << UpdateCollapsedPaths


dispatchUpdateViewResolvedComments : (Msg -> msg) -> Bool -> Cmd msg
dispatchUpdateViewResolvedComments wrapMsg =
    Task.dispatch << wrapMsg << UpdateViewResolvedComments


dispatchUpdateViewNamedOnlyVersions : (Msg -> msg) -> Bool -> Cmd msg
dispatchUpdateViewNamedOnlyVersions wrapMsg =
    Task.dispatch << wrapMsg << UpdateViewNamedOnlyVersions


dispatchUpdateViewSettings : (Msg -> msg) -> QuestionnaireViewSettings -> Cmd msg
dispatchUpdateViewSettings wrapMsg =
    Task.dispatch << wrapMsg << UpdateViewSettings


dispatchUpdateRightPanel : (Msg -> msg) -> QuestionnaireRightPanel -> Cmd msg
dispatchUpdateRightPanel wrapMsg =
    Task.dispatch << wrapMsg << UpdateRightPanel


dispatchOpenProjectActionModal : (Msg -> msg) -> ProjectCommon -> Uuid -> PluginElement -> Cmd msg
dispatchOpenProjectActionModal wrapMsg projectCommon pluginUuid pluginElement =
    Task.dispatch <| wrapMsg <| PluginProjectActionModalMsg <| PluginModal.open { pluginUuid = pluginUuid, pluginElement = pluginElement, data = projectCommon }


dispatchOpenPluginQuestionActionModal : (Msg -> msg) -> ProjectCommon -> Uuid -> PluginElement -> Question -> String -> Cmd msg
dispatchOpenPluginQuestionActionModal wrapMsg projectCommon pluginUuid pluginElement question questionPath =
    Task.dispatch <| wrapMsg <| PluginProjectQuestionActionModalMsg <| PluginModal.open { pluginUuid = pluginUuid, pluginElement = pluginElement, data = ( projectCommon, question, questionPath ) }


dispatchOpenPluginQuestionActionRightPanel : (Msg -> msg) -> PluginQuestionActionData -> Cmd msg
dispatchOpenPluginQuestionActionRightPanel wrapMsg pluginQuestionActionData =
    Task.dispatch <| wrapMsg <| UpdateRightPanel <| QuestionnaireRightPanel.PluginQuestionAction pluginQuestionActionData


dispatchSetLabels : (Msg -> msg) -> List String -> String -> Cmd msg
dispatchSetLabels wrapMsg labels path =
    Task.dispatch <| wrapMsg <| SetLabels path labels


dispatchSetFullScreen : (Msg -> msg) -> Bool -> Cmd msg
dispatchSetFullScreen wrapMsg =
    Task.dispatch << wrapMsg << SetFullScreen


applyProjectEvent : ProjectEvent -> Model -> Model
applyProjectEvent projectEvent model =
    let
        newModel1 =
            case projectEvent of
                ProjectEvent.SetReply setReplyData ->
                    { model | questionnaire = ProjectQuestionnaire.setReply setReplyData.path (SetReplyData.toReply setReplyData) model.questionnaire }

                ProjectEvent.ClearReply clearReplyData ->
                    { model | questionnaire = ProjectQuestionnaire.clearReplyValue clearReplyData.path model.questionnaire }

                ProjectEvent.SetPhase setPhaseData ->
                    { model | questionnaire = ProjectQuestionnaire.setPhaseUuid setPhaseData.phaseUuid model.questionnaire }

                ProjectEvent.SetLabels setLabelsData ->
                    { model | questionnaire = ProjectQuestionnaire.setLabels setLabelsData.path setLabelsData.value model.questionnaire }

                ProjectEvent.ResolveCommentThread resolveCommentThreadData ->
                    resolveCommentThread resolveCommentThreadData model

                ProjectEvent.ReopenCommentThread reopenCommentThreadData ->
                    reopenCommentThread reopenCommentThreadData model

                ProjectEvent.DeleteCommentThread deleteCommentThreadData ->
                    deleteCommentThread deleteCommentThreadData model

                ProjectEvent.AssignCommentThread assignCommentThreadData ->
                    assignCommentThread assignCommentThreadData model

                ProjectEvent.AddComment addCommentData ->
                    addComment addCommentData model

                ProjectEvent.EditComment editCommentData ->
                    editComment editCommentData model

                ProjectEvent.DeleteComment deleteCommentData ->
                    deleteComment deleteCommentData model

        newModel2 =
            case model.rightPanel of
                QuestionnaireRightPanel.VersionHistory ->
                    { newModel1 | versionHistoryRightPanelModel = VersionHistoryRightPanel.addEvent projectEvent model.versionHistoryRightPanelModel }

                _ ->
                    newModel1
    in
    newModel2
        |> virtualizeContentIfNeeded projectEvent
        |> calculateUnansweredQuestionsIfNeeded projectEvent
        |> updateWarningsIfNeeded projectEvent
        |> updateTodosIfNeeded projectEvent


updateQuestionnaire : (ProjectQuestionnaire -> ProjectQuestionnaire) -> Model -> Model
updateQuestionnaire fn model =
    { model | questionnaire = fn model.questionnaire }


resolveCommentThread : ResolveCommentThreadData -> Model -> Model
resolveCommentThread data model =
    let
        mapCommentThread commentThread =
            { commentThread | resolved = True }
    in
    model
        |> mapCommentThreads data.path (List.map (wrapMapCommentThread data.threadUuid mapCommentThread))
        |> updateQuestionnaire (ProjectQuestionnaire.addResolvedCommentThreadToCount data.path data.threadUuid data.commentCount)


reopenCommentThread : ReopenCommentThreadData -> Model -> Model
reopenCommentThread data model =
    let
        mapCommentThread commentThread =
            { commentThread | resolved = False }
    in
    model
        |> mapCommentThreads data.path (List.map (wrapMapCommentThread data.threadUuid mapCommentThread))
        |> updateQuestionnaire (ProjectQuestionnaire.addReopenedCommentThreadToCount data.path data.threadUuid data.commentCount)


deleteCommentThread : DeleteCommentThreadData -> Model -> Model
deleteCommentThread data model =
    model
        |> mapCommentThreads data.path (List.filter (\t -> t.uuid /= data.threadUuid))
        |> updateQuestionnaire (ProjectQuestionnaire.removeCommentThreadFromCount data.path data.threadUuid)


assignCommentThread : AssignCommentThreadData -> Model -> Model
assignCommentThread data model =
    let
        mapCommentThread commentThread =
            { commentThread | assignedTo = data.assignedTo }
    in
    model
        |> mapCommentThreads data.path (List.map (wrapMapCommentThread data.threadUuid mapCommentThread))


addComment : AddCommentData -> Model -> Model
addComment data model =
    let
        threadExists =
            Dict.get data.path model.commentThreadsMap
                |> Maybe.andThen ActionResult.toMaybe
                |> Maybe.withDefault []
                |> List.any (.uuid >> (==) data.threadUuid)

        comment =
            AddCommentData.toComment data

        mapCommentThread commentThread =
            { commentThread | comments = commentThread.comments ++ [ comment ] }

        questionnaireWithThread =
            if threadExists then
                model

            else
                addCommentThread data.path data.threadUuid data.private comment model
    in
    questionnaireWithThread
        |> mapCommentThreads data.path (List.map (wrapMapCommentThread data.threadUuid mapCommentThread))
        |> updateQuestionnaire (ProjectQuestionnaire.addCommentCount data.path data.threadUuid)


addCommentThread : String -> Uuid -> Bool -> Comment -> Model -> Model
addCommentThread path threadUuid private comment model =
    let
        commentThread =
            { uuid = threadUuid
            , resolved = False
            , comments = []
            , private = private
            , createdAt = comment.createdAt
            , createdBy = comment.createdBy
            , assignedTo = Nothing
            }

        commentThreads =
            Dict.get path model.commentThreadsMap
                |> Maybe.withDefault (Success [])

        mapAddCommentThread originalCommentThreads =
            originalCommentThreads ++ [ commentThread ]
    in
    { model | commentThreadsMap = Dict.insert path (ActionResult.map mapAddCommentThread commentThreads) model.commentThreadsMap }


editComment : EditCommentData -> Model -> Model
editComment data =
    let
        mapComment comment =
            if comment.uuid == data.commentUuid then
                { comment | text = data.text, updatedAt = data.createdAt }

            else
                comment

        mapCommentThread commentThread =
            { commentThread | comments = List.map mapComment commentThread.comments }
    in
    mapCommentThreads data.path (List.map (wrapMapCommentThread data.threadUuid mapCommentThread))


deleteComment : DeleteCommentData -> Model -> Model
deleteComment data model =
    let
        mapCommentThread commentThread =
            { commentThread | comments = List.filter (\c -> c.uuid /= data.commentUuid) commentThread.comments }
    in
    model
        |> mapCommentThreads data.path (List.map (wrapMapCommentThread data.threadUuid mapCommentThread))
        |> updateQuestionnaire (ProjectQuestionnaire.subCommentCount data.path data.threadUuid)


mapCommentThreads : String -> (List CommentThread -> List CommentThread) -> Model -> Model
mapCommentThreads path map model =
    let
        mbCommentThreads =
            Dict.get path model.commentThreadsMap
                |> Maybe.map (ActionResult.map map)
    in
    case mbCommentThreads of
        Just commentThreads ->
            { model | commentThreadsMap = Dict.insert path commentThreads model.commentThreadsMap }

        Nothing ->
            model


wrapMapCommentThread : Uuid -> (CommentThread -> CommentThread) -> CommentThread -> CommentThread
wrapMapCommentThread threadUuid mapCommentThread commentThread =
    if commentThread.uuid == threadUuid then
        mapCommentThread commentThread

    else
        commentThread


virtualizeContentIfNeeded : ProjectEvent -> Model -> Model
virtualizeContentIfNeeded projectEvent model =
    if QuestionnaireVirtualization.needVirtualization projectEvent then
        virtualizeContent model

    else
        model


virtualizeContent : Model -> Model
virtualizeContent model =
    { model
        | content =
            QuestionnaireVirtualization.virtualizeChapter
                { chapterUuid = model.chapterUuid
                , questionnaire = model.questionnaire
                , collapsedPaths = model.collapsedPaths
                , resourcePageToUrl = Wizard.Routes.knowledgeModelsResourcePage model.questionnaire.knowledgeModelPackage.uuid
                , viewSettings = model.viewSettings
                }
    }


calculateUnansweredQuestionsIfNeeded : ProjectEvent -> Model -> Model
calculateUnansweredQuestionsIfNeeded projectEvent model =
    let
        needsRecalculation =
            case projectEvent of
                ProjectEvent.SetReply _ ->
                    True

                ProjectEvent.ClearReply _ ->
                    True

                _ ->
                    False
    in
    if needsRecalculation then
        calculateUnansweredQuestions model

    else
        model


calculateUnansweredQuestions : Model -> Model
calculateUnansweredQuestions model =
    let
        chapterUnansweredQuestions =
            model.questionnaire.knowledgeModel.chapterUuids
                |> List.map
                    (\chapterUuid ->
                        ( chapterUuid, ProjectQuestionnaire.calculateUnansweredQuestionsForChapter model.questionnaire chapterUuid )
                    )
                |> Dict.fromList
    in
    { model | chapterUnansweredQuestions = chapterUnansweredQuestions }


updateTodosIfNeeded : ProjectEvent -> Model -> Model
updateTodosIfNeeded projectEvent model =
    let
        needsUpdate =
            case projectEvent of
                ProjectEvent.SetLabels _ ->
                    True

                ProjectEvent.SetReply reply ->
                    case reply.value of
                        ReplyValue.AnswerReply _ ->
                            True

                        ReplyValue.ItemListReply _ ->
                            True

                        _ ->
                            False

                ProjectEvent.ClearReply _ ->
                    True

                _ ->
                    False
    in
    if needsUpdate then
        { model | todos = ProjectQuestionnaire.getTodos model.questionnaire }

    else
        model


updateWarningsIfNeeded : ProjectEvent -> Model -> Model
updateWarningsIfNeeded projectEvent model =
    let
        needsUpdate =
            case projectEvent of
                ProjectEvent.SetReply reply ->
                    case reply.value of
                        ReplyValue.StringReply _ ->
                            True

                        _ ->
                            False

                ProjectEvent.ClearReply _ ->
                    True

                _ ->
                    False
    in
    if needsUpdate then
        { model | warnings = ProjectQuestionnaire.getWarnings model.questionnaire }

    else
        model


contentElementSelector : String
contentElementSelector =
    ".questionnaire2__content"


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        toolbarSub =
            Sub.map ToolbarMsg <|
                Toolbar.subscriptions model.toolbarModel

        contentSplitPaneSub =
            Sub.map ContentSplitPaneMsg <|
                SplitPane.subscriptions model.contentSplitPane

        contentScrollSub =
            Dom.gotScrollTop GotContentScrollTop

        rightPanelSplitPaneSub =
            Sub.map RightPanelSplitPaneMsg <|
                SplitPane.subscriptions model.rightPanelSplitPane

        localStorageDataSub =
            LocalStorage.gotItemRaw LocalStorageGotData

        rightPanelSub =
            case model.rightPanel of
                QuestionnaireRightPanel.Comments _ ->
                    Sub.map CommentsRightPanelMsg (CommentsRightPanel.subscriptions model.commentsRightPanelModel)

                QuestionnaireRightPanel.VersionHistory ->
                    Sub.map VersionHistoryRightPanelMsg (VersionHistoryRightPanel.subscriptions model.versionHistoryRightPanelModel)

                _ ->
                    Sub.none
    in
    Sub.batch
        [ toolbarSub
        , contentSplitPaneSub
        , contentScrollSub
        , rightPanelSplitPaneSub
        , localStorageDataSub
        , rightPanelSub
        ]


type alias ViewConfig msg =
    { wrapMsg : Msg -> msg
    , readonly : Bool
    , actionsEnabled : Bool
    , toolbarEnabled : Bool
    , previewQuestionnaireEventMsg : Maybe (Uuid -> msg)
    , revertQuestionnaireMsg : Maybe (ProjectEvent -> msg)
    }


view : AppState -> ViewConfig msg -> Model -> Html msg
view appState cfg model =
    let
        toolbar =
            Html.viewIf cfg.toolbarEnabled <|
                Html.map (cfg.wrapMsg << ToolbarMsg) <|
                    Toolbar.view appState
                        model.viewSettings
                        model.rightPanel
                        model.questionnaire
                        model.todos
                        model.warnings
                        model.toolbarModel

        contentSplitPaneConfig =
            SplitPane.createViewConfig
                { toMsg = cfg.wrapMsg << ContentSplitPaneMsg
                , customSplitter = Nothing
                }

        rightPanelActive =
            model.rightPanel /= QuestionnaireRightPanel.None

        ( rightPanelSplitPaneConfig, rightPanelSplitPaneState ) =
            if rightPanelActive then
                ( SplitPane.createViewConfig
                    { toMsg = cfg.wrapMsg << RightPanelSplitPaneMsg
                    , customSplitter = Nothing
                    }
                , model.rightPanelSplitPane
                )

            else
                ( SplitPane.createViewConfig
                    { toMsg = cfg.wrapMsg << RightPanelSplitPaneMsg
                    , customSplitter =
                        Just
                            (SplitPane.createCustomSplitter (cfg.wrapMsg << RightPanelSplitPaneMsg)
                                { attributes = []
                                , children = []
                                }
                            )
                    }
                , model.rightPanelClosedSplitPane
                )

        questionnaireContentViewConfig =
            { collapsedPaths = model.collapsedPaths
            , content = model.content
            , featuresEnabled = cfg.toolbarEnabled
            , mbHighlightedPath = model.mbHighlightedPath
            , questionnaire = model.questionnaire
            , rightPanel = model.rightPanel
            , showActions = cfg.actionsEnabled
            , readonly = cfg.readonly
            , viewSettings = model.viewSettings
            }

        questionnaireContent =
            Html.map (cfg.wrapMsg << QuestionnaireContentMsg) <|
                QuestionnaireContent.view appState questionnaireContentViewConfig model.contentModel

        leftPanelPhaseSelection =
            Html.map (cfg.wrapMsg << PhaseSelectionMsg) <|
                PhaseSelection.viewPhaseSelection appState model.questionnaire cfg.readonly

        leftPanelNavigationTree =
            Html.map (cfg.wrapMsg << NavigationTreeMsg) <|
                NavigationTree.view
                    appState.locale
                    model.questionnaire
                    model.chapterUuid
                    model.viewSettings.nonDesirableQuestions
                    model.collapsedPaths
                    model.chapterUnansweredQuestions

        questionnaire =
            SplitPane.view contentSplitPaneConfig
                (div [ class "questionnaire2__left-panel" ]
                    [ leftPanelPhaseSelection
                    , leftPanelNavigationTree
                    ]
                )
                (div [ class "questionnaire2__content" ]
                    [ questionnaireContent
                    ]
                )
                model.contentSplitPane

        rightPanel =
            case model.rightPanel of
                QuestionnaireRightPanel.None ->
                    Html.nothing

                QuestionnaireRightPanel.Search ->
                    Html.map cfg.wrapMsg <|
                        SearchRightPanel.view appState
                            { scrollToPathMsg = ScrollToPath False
                            , scrollToQuestionMsg = ScrollToQuestion
                            , openChapterMsg = OpenChapter True
                            , wrapMsg = SearchRightPanelMsg
                            }
                            model.searchRightPanelModel

                QuestionnaireRightPanel.TODOs ->
                    Html.map (cfg.wrapMsg << TodosRightPanelMsg) <|
                        TodosRightPanel.view appState.locale model.todos

                QuestionnaireRightPanel.VersionHistory ->
                    VersionHistoryRightPanel.view appState
                        { namedOnly = model.viewNamedOnlyVersions
                        , onToggleNamedOnly = cfg.wrapMsg << UpdateViewNamedOnlyVersions
                        , previewQuestionnaireEventMsg = cfg.previewQuestionnaireEventMsg
                        , questionnaire = model.questionnaire
                        , revertQuestionnaireMsg = cfg.revertQuestionnaireMsg
                        , scrollMsg = cfg.wrapMsg << ScrollToPath False
                        , wrapMsg = cfg.wrapMsg << VersionHistoryRightPanelMsg
                        }
                        model.versionHistoryRightPanelModel

                QuestionnaireRightPanel.CommentsOverview ->
                    Html.map cfg.wrapMsg <|
                        CommentsRightPanel.viewCommentsOverview
                            { locale = appState.locale
                            , onOpenComments = OpenComments
                            , onToggleCommentsViewResolved = UpdateViewResolvedComments
                            , questionnaire = model.questionnaire
                            , viewResolved = model.viewResolvedComments
                            }

                QuestionnaireRightPanel.Comments questionPath ->
                    let
                        commentThreadsResult =
                            Dict.get questionPath model.commentThreadsMap
                                |> Maybe.withDefault ActionResult.Loading
                    in
                    Html.map cfg.wrapMsg <|
                        CommentsRightPanel.viewQuestionComments appState
                            { onOpenComments = OpenComments
                            , onToggleCommentsViewResolved = UpdateViewResolvedComments
                            , questionnaire = model.questionnaire
                            , questionPath = questionPath
                            , viewResolved = model.viewResolvedComments
                            , wrapMsg = CommentsRightPanelMsg
                            }
                            model.commentsRightPanelModel
                            commentThreadsResult

                QuestionnaireRightPanel.Warnings ->
                    Html.map (cfg.wrapMsg << WarningsRightPanelMsg) <|
                        WarningsRightPanel.view appState.locale model.warnings

                QuestionnaireRightPanel.PluginQuestionAction pluginAction ->
                    div [] [ text ("Plugin Action: " ++ pluginAction.plugin.name) ]

        body =
            SplitPane.view rightPanelSplitPaneConfig
                questionnaire
                rightPanel
                rightPanelSplitPaneState

        pluginProjectActionModal =
            PluginModal.view appState
                { attributes = \project -> [ PluginElement.projectValue project ]
                , wrapMsg = cfg.wrapMsg << PluginProjectActionModalMsg
                }
                model.pluginProjectActionModalModel

        pluginProjectQuestionActionModal =
            PluginModal.view appState
                { attributes =
                    \( project, question, questionPath ) ->
                        [ PluginElement.projectValue project
                        , PluginElement.questionValue question
                        , PluginElement.questionPathValue questionPath
                        ]
                , wrapMsg = cfg.wrapMsg << PluginProjectQuestionActionModalMsg
                }
                model.pluginProjectQuestionActionModalModel

        searchShortcut =
            Shortcut.primaryShortcut appState.navigator.isMac (Shortcut.Regular "f") (cfg.wrapMsg (UpdateRightPanel QuestionnaireRightPanel.Search))

        closePanelShortcut =
            if model.rightPanel /= QuestionnaireRightPanel.None then
                [ Shortcut.esc (cfg.wrapMsg (UpdateRightPanel QuestionnaireRightPanel.None)) ]

            else
                []

        shortcuts =
            searchShortcut :: closePanelShortcut
    in
    Shortcut.shortcutElement shortcuts
        [ class "questionnaire2"
        , classList [ ( "questionnaire2--toolbarEnabled", cfg.toolbarEnabled ) ]
        ]
        [ toolbar
        , div
            [ class "questionnaire2__body", dataTour "questionnaire_body" ]
            [ body
            ]
        , Html.map (cfg.wrapMsg << FileUploadModalMsg) <| FileUploadModal.view appState False model.fileUploadModalModel
        , Html.map (cfg.wrapMsg << FileDeleteModalMsg) <| FileDeleteModal.view appState model.fileDeleteModalModel
        , Html.map (cfg.wrapMsg << PhaseSelectionMsg) <| PhaseSelection.viewPhaseModal appState model.questionnaire model.phaseSelectionModel
        , pluginProjectActionModal
        , pluginProjectQuestionActionModal
        ]
