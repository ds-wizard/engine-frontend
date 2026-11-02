module Common.Components.Questionnaire exposing
    ( Model
    , Msg
    , UpdateConfig
    , ViewConfig
    , addFile
    , applyProjectEvent
    , hasFiles
    , init
    , initSimple
    , navToggle
    , openChapterMsg
    , openCommentsMsg
    , restoreScrollMsg
    , scrollToPathMsg
    , setFiles
    , setKnowledgeModel
    , setPhaseMsg
    , setReplies
    , subscriptions
    , update
    , updateWithQuestionnaireData
    , updateWithShareData
    , view
    )

{-| A project's questionnaire: the chapters of its knowledge model as questions to
answer, the answers given, and everything that moves between them.

Used by the project's questionnaire tab (editable, collaborative), the knowledge
model preview, the project import and the version view (read-only), and the
knowledge model editor's preview.

**The workspace.** A `Ui.Page.Workspace`: the toolbar above, the phase and the
chapter navigation down the start, the chapter in the middle, and a panel down the
end while one is open. On a narrow page the navigation and the panel open over the
chapter instead.

**Performance is the design.** The current chapter is built
once into nodes (`Nodes.build`) and kept here, and so is the navigation's tree
(`ChapterNav.build`); both are built again only when the chapter's shape can have
changed (`Rebuild`). Every node renders through `Html.Lazy` with stable or
primitive arguments (`Content`, `Question`), the content sits behind one lazy call
whose arguments are all held here or primitive, and the navigation and both halves
of the toolbar are lazy over what they show — so a render with nothing new in it
(the app refreshing the time, a settled scroll) stops at the top of each, and a
keystroke reaches only the question it was typed into.

**Events.** A change the user makes comes back from `update` as a `ProjectEvent`,
already applied here; the project page sends it. An event from elsewhere — the
websocket, an importer — goes through `applyProjectEvent`.

**Moving to a question** — a deep link, the navigation, a search result — expands
everything above it, scrolls it into view and moves focus to its title, and
highlights it for three seconds (visual only; the focus is what says where the
reader is).

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.BootstrapConfig.UserConfig as UserConfig
import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.KnowledgeModel.Question exposing (Question)
import Common.Api.Models.Permission exposing (Permission)
import Common.Api.Models.Project.ProjectSharing exposing (ProjectSharing)
import Common.Api.Models.Project.ProjectTodo exposing (ProjectTodo)
import Common.Api.Models.Project.ProjectVisibility exposing (ProjectVisibility)
import Common.Api.Models.ProjectCommon exposing (ProjectCommon)
import Common.Api.Models.ProjectDetail.CommentThread exposing (CommentThread)
import Common.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Common.Api.Models.ProjectDetail.ProjectEvent.SetReplyData as SetReplyData
import Common.Api.Models.ProjectDetail.Reply exposing (Reply)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue
import Common.Api.Models.ProjectFileSimple exposing (ProjectFileSimple)
import Common.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire, QuestionCommentInfo, QuestionnaireUnansweredQuestion, QuestionnaireWarning)
import Common.Api.Models.ProjectVersion exposing (ProjectVersion)
import Common.Api.Models.WebSockets.ProjectMessage.SetProjectData exposing (SetProjectData)
import Common.Api.Projects as ProjectsApi
import Common.Components.PluginModal as PluginModal
import Common.Components.PluginView as PluginView
import Common.Components.Questionnaire.ChapterNav as ChapterNav exposing (ChapterNav)
import Common.Components.Questionnaire.Content as Content
import Common.Components.Questionnaire.ContentMsg as ContentMsg
import Common.Components.Questionnaire.Flags as Flags
import Common.Components.Questionnaire.LocalStorage as LocalStorage
import Common.Components.Questionnaire.Modal.DeleteVersion as DeleteVersionModal
import Common.Components.Questionnaire.Modal.FileDelete as FileDelete
import Common.Components.Questionnaire.Modal.FileUpload as FileUpload
import Common.Components.Questionnaire.Modal.Version as VersionModal
import Common.Components.Questionnaire.Nest as Nest
import Common.Components.Questionnaire.Nodes as Nodes
import Common.Components.Questionnaire.Panel.Comments as Comments
import Common.Components.Questionnaire.Panel.CommentsOverview as CommentsOverview
import Common.Components.Questionnaire.Panel.Group as Group
import Common.Components.Questionnaire.Panel.Lists as Lists
import Common.Components.Questionnaire.Panel.Search as Search
import Common.Components.Questionnaire.Panel.VersionHistory as VersionHistory
import Common.Components.Questionnaire.Path as Path
import Common.Components.Questionnaire.PhaseSelection as PhaseSelection
import Common.Components.Questionnaire.Rebuild as Rebuild
import Common.Components.Questionnaire.RightPanel as RightPanel exposing (RightPanel)
import Common.Components.Questionnaire.Toolbar as Toolbar
import Common.Components.Questionnaire.ViewSettings as ViewSettings exposing (ViewSettings)
import Common.Components.UiWords as UiWords
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Plugins.Plugin as Plugin
import Common.Plugins.PluginElement as PluginElement exposing (PluginElement)
import Common.Ports.Dom as Dom
import Common.Ports.LocalStorage as LocalStoragePort
import Common.Routes as Routes
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Dict exposing (Dict)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (attribute, class, style)
import Html.Keyed
import Html.Lazy as Lazy
import Json.Encode as E
import List.Extra as List
import Maybe.Extra as Maybe
import Process
import Random exposing (Seed)
import Set exposing (Set)
import Shortcut
import Task
import Time
import Ui.Page.Workspace as Workspace
import Ui.Resizer as Resizer
import Uuid exposing (Uuid)
import Uuid.Extra as Uuid



-- MODEL


type alias Model =
    { uuid : Uuid
    , questionnaire : ProjectQuestionnaire

    -- The questionnaire's language: what the knowledge model's own texts are
    -- translated into, which is not the application's.
    , kmLocale : Gettext.Locale

    -- The address of one of the knowledge model's resource pages, with
    -- `resourcePagePlaceholder` where its uuid goes: the nodes are rebuilt
    -- without the application state, which is what knows how a page is
    -- addressed.
    , resourcePageUrl : String
    , chapterUuid : String
    , collapsedPaths : Set String
    , viewSettings : ViewSettings
    , knowledgeModelParentMap : KnowledgeModel.ParentMap

    -- Derived, and each kept up to date only as far as an event can move it
    -- (`Rebuild`), so their references hold when nothing changed.
    , chapterUnansweredCounts : Dict String Int
    , todos : List ProjectTodo
    , warnings : List QuestionnaireWarning

    -- The unanswered questions, kept only while the panel that shows them is
    -- open: working them out is a walk of the whole questionnaire (§11.2).
    , unanswered : Maybe (List QuestionnaireUnansweredQuestion)
    , commentsCount : Int

    -- Where the comments are, for the overview; kept only while it is open.
    , commentInfos : Maybe (List QuestionCommentInfo)

    -- A question's threads, once they have been asked for.
    , commentThreads : Dict String (ActionResult (List CommentThread))
    , nav : ChapterNav

    -- The workspace.
    , rightPanel : RightPanel
    , navOverlayOpen : Bool
    , navResizer : Resizer.Model
    , asideResizer : Resizer.Model
    , toolbar : Toolbar.Model
    , search : Search.Model
    , comments : Comments.Model
    , versionHistory : VersionHistory.Model
    , versionModal : VersionModal.Model
    , deleteVersionModal : DeleteVersionModal.Model
    , phaseModalOpen : Bool
    , viewResolvedComments : Bool
    , viewNamedOnlyVersions : Bool
    , contentScrollTop : Maybe Int
    , commentThreadUuid : Maybe Uuid

    -- Components.
    , content : Content.Model
    , fileUpload : FileUpload.Model
    , fileDelete : FileDelete.Model
    , projectActionModal : PluginModal.Model ProjectCommon
    , questionActionModal : PluginModal.Model ( ProjectCommon, Question, String )

    -- Everything the content is drawn from besides its own model, in one record
    -- that is only replaced when something in it changes (`setViewState`).
    , viewState : Content.ViewState

    -- What this questionnaire's fixed ids start with; see `workspaceId`.
    , idPrefix : String
    }


{-| A questionnaire only to be read — a knowledge model's preview, or a version
as it was. Its ids are its own, so it can be shown over one being worked on.
-}
initSimple : AppState -> ProjectQuestionnaire -> ( Model, Cmd Msg )
initSimple appState questionnaire =
    let
        ( model, cmd ) =
            init appState questionnaire Nothing Nothing
    in
    ( { model | idPrefix = "questionnaire-preview" }, cmd )


{-| `mbPath` and `mbCommentThreadUuid` are the questionnaire tab's deep link: the
question to scroll to, and the comment thread to open on it.
-}
init : AppState -> ProjectQuestionnaire -> Maybe String -> Maybe Uuid -> ( Model, Cmd Msg )
init appState questionnaire mbPath mbCommentThreadUuid =
    let
        chapterUuid =
            case mbPath of
                Just path ->
                    Path.chapterUuid path

                Nothing ->
                    List.head questionnaire.knowledgeModel.chapterUuids
                        |> Maybe.withDefault ""

        questionActionPlugins =
            AppState.getPluginsByConnector appState .projectQuestionActions
                |> Plugin.filterByKmPatterns (KnowledgeModelUtils.getPackageId questionnaire.knowledgeModelPackage)
                |> List.sortBy (Tuple.second >> .action >> .name)

        kmLocale =
            Maybe.withDefault Gettext.defaultLocale questionnaire.locale

        -- The comments of a question, when the link names a thread on it; the
        -- panel stored for the project otherwise, once local storage answers.
        rightPanel =
            case ( mbPath, mbCommentThreadUuid ) of
                ( Just path, Just _ ) ->
                    RightPanel.Comments path

                _ ->
                    RightPanel.None

        model =
            { uuid = questionnaire.uuid
            , questionnaire = questionnaire
            , kmLocale = kmLocale
            , resourcePageUrl = Routes.toUrl appState (Routes.knowledgeModelsResourcePage questionnaire.knowledgeModelPackage.uuid resourcePagePlaceholder)
            , chapterUuid = chapterUuid
            , collapsedPaths = Set.empty
            , viewSettings = ViewSettings.default
            , knowledgeModelParentMap = KnowledgeModel.createParentMap questionnaire.knowledgeModel
            , chapterUnansweredCounts = Rebuild.chapterUnansweredCounts questionnaire
            , todos = ProjectQuestionnaire.getTodos questionnaire
            , warnings = ProjectQuestionnaire.getWarnings questionnaire
            , unanswered = Nothing
            , commentsCount = ProjectQuestionnaire.commentsLength questionnaire
            , commentInfos = Nothing
            , commentThreads = Dict.empty
            , nav = { chapters = [], activeChapterUuid = chapterUuid, active = Nothing }
            , rightPanel = rightPanel
            , navOverlayOpen = False
            , navResizer = Resizer.init (navResizerConfig appState)
            , asideResizer = Resizer.init (asideResizerConfig appState)
            , toolbar = Toolbar.init appState questionnaire
            , search = Search.init
            , comments = Comments.init questionnaire.uuid
            , versionHistory = VersionHistory.init questionnaire.uuid
            , versionModal = VersionModal.init questionnaire.uuid
            , deleteVersionModal = DeleteVersionModal.init questionnaire.uuid
            , phaseModalOpen = False
            , viewResolvedComments = False
            , viewNamedOnlyVersions = False
            , contentScrollTop = Nothing
            , commentThreadUuid = mbCommentThreadUuid
            , content = Content.init appState questionActionPlugins
            , fileUpload = FileUpload.init questionnaire.uuid
            , fileDelete = FileDelete.init questionnaire.uuid
            , projectActionModal = PluginModal.initialModel
            , questionActionModal = PluginModal.initialModel
            , viewState =
                { structure = Content.structure []
                , questionnaire = questionnaire
                , highlightedPath = Nothing
                , commentsOpenPath = RightPanel.commentsPath rightPanel
                , openActions = Nothing
                }
            , idPrefix = "questionnaire"
            }
                |> rebuild

        ( scrolled, scrollCmd ) =
            case mbPath of
                Just path ->
                    scrollToPath True path model

                Nothing ->
                    ( model, Cmd.none )
    in
    ( scrolled
    , Cmd.batch
        [ LocalStorage.getItems questionnaire.uuid (rightPanel == RightPanel.None)
        , Dom.watchScrollTop (mainSelector scrolled)
        , scrollCmd

        -- The panel is open on arrival when the link names a thread, and a
        -- panel nobody opened is a panel nobody asked the server for.
        , case rightPanel of
            RightPanel.Comments path ->
                fetchThreads appState path scrolled

            _ ->
                Cmd.none
        ]
    )


{-| The navigation is resizable from its trailing edge, 16 to 40rem.
-}
navResizerConfig : AppState -> Resizer.Config
navResizerConfig appState =
    { edge = Resizer.Trailing
    , label = gettext "Resize chapters" appState.locale
    , min = 224
    , max = 560
    , default = 288
    , collapse = Nothing
    }


asideResizerConfig : AppState -> Resizer.Config
asideResizerConfig appState =
    { edge = Resizer.Leading
    , label = gettext "Resize panel" appState.locale
    , min = 288
    , max = 800
    , default = 384
    , collapse = Nothing
    }


{-| Everything with a fixed id in a questionnaire is named after its own
instance, because two can be on the page at once: the project being worked on,
and, over it, the same project as it was at some change.
-}
workspaceId : Model -> String
workspaceId model =
    model.idPrefix


{-| What a page needs to draw the control for the questionnaire's own column.

A page that hides the questionnaire's toolbar hides the button that folds the
chapters away with it, and on a narrow screen that column has to be reachable
from somewhere. This says whether it is open, which element it controls and what
opens it, so the page can put the control in a row of its own making — the
importer puts it beside the one for its changes.

-}
navToggle : Model -> { open : Bool, controls : String, onToggle : Bool -> Msg }
navToggle model =
    { open = model.navOverlayOpen
    , controls = Workspace.navId (workspaceId model)
    , onToggle = SetNavOverlay
    }


{-| The chapter's scroll container.
-}
mainSelector : Model -> String
mainSelector model =
    "#" ++ Workspace.mainId (workspaceId model)


{-| Build the current chapter's nodes and the navigation's tree again, from the
questionnaire as it is now.
-}
rebuild : Model -> Model
rebuild model =
    let
        nodes =
            Nodes.build
                { chapterUuid = model.chapterUuid
                , questionnaire = model.questionnaire
                , collapsedPaths = model.collapsedPaths
                , resourcePageUrl = \resourcePageUuid -> String.replace resourcePagePlaceholder resourcePageUuid model.resourcePageUrl
                , viewSettings = model.viewSettings
                , locale = model.kmLocale
                , knowledgeModelParentMap = model.knowledgeModelParentMap
                , integrationPlugins = model.content.integrationPlugins
                , builtInIntegrations = model.content.builtIns
                }

        nav =
            ChapterNav.build
                { questionnaire = model.questionnaire
                , chapterUuid = model.chapterUuid
                , collapsedPaths = model.collapsedPaths
                , nonDesirableQuestions = model.viewSettings.nonDesirableQuestions
                , kmLocale = model.kmLocale
                }
    in
    { model | nav = nav }
        |> setViewState (\state -> { state | structure = Content.structure nodes })


setViewState : (Content.ViewState -> Content.ViewState) -> Model -> Model
setViewState change model =
    { model | viewState = change model.viewState }


setQuestionnaire : ProjectQuestionnaire -> Model -> Model
setQuestionnaire questionnaire model =
    { model | questionnaire = questionnaire }
        |> setViewState (\state -> { state | questionnaire = questionnaire })


{-| The answers given, replaced wholesale.

The knowledge model editor's preview keeps its values on the editor itself, so
a save — mine or another editor's — arrives over the websocket and replaces
what is here.

-}
setReplies : Dict String Reply -> Model -> Model
setReplies replies model =
    let
        questionnaire =
            model.questionnaire

        withReplies =
            { questionnaire | replies = replies }
    in
    setQuestionnaire withReplies
        { model
            | chapterUnansweredCounts = Rebuild.chapterUnansweredCounts withReplies
            , todos = ProjectQuestionnaire.getTodos withReplies
            , warnings = ProjectQuestionnaire.getWarnings withReplies
        }
        |> rebuild


{-| The knowledge model the questions come from, replaced in place.

The knowledge model editor's preview narrows the model by question tag, and a
tag ticked has to change what is shown without losing where the reader is: the
chapter open, what is collapsed, the replies given. Everything built from the
model is built again, which is the whole cost — the chapter's nodes, the
navigation, and the counts beside it.

Tags take questions away, never chapters, so the chapter held here is still a
chapter afterwards.

-}
setKnowledgeModel : KnowledgeModel.KnowledgeModel -> Model -> Model
setKnowledgeModel knowledgeModel model =
    let
        questionnaire =
            model.questionnaire

        withKnowledgeModel =
            { questionnaire | knowledgeModel = knowledgeModel }
    in
    setQuestionnaire withKnowledgeModel
        { model
            | knowledgeModelParentMap = KnowledgeModel.createParentMap knowledgeModel
            , chapterUnansweredCounts = Rebuild.chapterUnansweredCounts withKnowledgeModel
            , todos = ProjectQuestionnaire.getTodos withKnowledgeModel
            , warnings = ProjectQuestionnaire.getWarnings withKnowledgeModel
        }
        |> rebuild



-- UPDATE


type Msg
    = ContentMsg ContentMsg.Msg
    | ChapterNavMsg ChapterNav.Msg
    | PhaseMsg PhaseSelection.Msg
    | ToolbarMsg Toolbar.Msg
    | SearchMsg Search.Msg
    | CommentsMsg Comments.Msg
    | CommentsOverviewMsg CommentsOverview.Msg
    | GotCommentThreads String (Result ApiError (Dict String (List CommentThread)))
    | SetViewResolved Bool
    | VersionHistoryMsg VersionHistory.Msg
    | VersionModalMsg VersionModal.Msg
    | DeleteVersionModalMsg DeleteVersionModal.Msg
    | VersionSaved ProjectVersion
    | VersionDeleted ProjectVersion
    | OpenChapter String
    | OpenCommentsLink String (Maybe Uuid)
    | ScrollToPath Bool String
    | ClearHighlightedPath String
    | UpdateCollapsedPaths (Set String)
    | SetViewSettings ViewSettings
    | SetRightPanel RightPanel
    | TogglePanel RightPanel
    | SetNavOverlay Bool
    | NavResizerMsg Resizer.Msg
    | AsideResizerMsg Resizer.Msg
    | LocalStorageGot E.Value
    | GotScrollTop Dom.ScrollTop
    | RestoreScroll
    | SetPhase (Maybe Uuid)
    | SetLabels String (List String)
    | FileUploadMsg FileUpload.Msg
    | FileDeleteMsg FileDelete.Msg
    | SetFile String ProjectFileSimple
    | RemoveFile String
    | OpenProjectAction Uuid PluginElement
    | ProjectActionModalMsg (PluginModal.Msg ProjectCommon)
    | OpenQuestionActionModal Uuid PluginElement Question String
    | QuestionActionModalMsg (PluginModal.Msg ( ProjectCommon, Question, String ))


openChapterMsg : String -> Msg
openChapterMsg =
    OpenChapter


{-| The comments of a question, as a link asks for them: the panel opens on the
question, the question is scrolled to, and the thread the link names is opened
once the threads have arrived.

For a link followed while the project is already open — `init` does the same for
one the project is opened by.

-}
openCommentsMsg : String -> Maybe Uuid -> Msg
openCommentsMsg =
    OpenCommentsLink


{-| Scroll to a path, expanding everything on the way to it; `instant` skips the
smooth scroll, for a deep link on arrival.
-}
scrollToPathMsg : Bool -> String -> Msg
scrollToPathMsg =
    ScrollToPath


setPhaseMsg : Maybe Uuid -> Msg
setPhaseMsg =
    SetPhase


{-| Back where the reader left the chapter, for a page that shows the
questionnaire again without building it again (the project's tab, returned to).
-}
restoreScrollMsg : Msg
restoreScrollMsg =
    RestoreScroll


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , mbKmEditorUuid : Maybe Uuid
    , projectCommon : ProjectCommon

    -- Viewing an old version and going back to one are the page's to do; the
    -- history panel only asks. `Nothing` where the page cannot.
    , previewVersionMsg : Maybe (Uuid -> String -> msg)
    , revertVersionMsg : Maybe (ProjectEvent -> msg)
    }


type alias Return msg =
    { seed : Seed, model : Model, cmd : Cmd msg, event : Maybe ProjectEvent }


update : AppState -> UpdateConfig msg -> Msg -> Model -> Return msg
update appState cfg msg model =
    let
        plain ( newModel, cmd ) =
            { seed = appState.seed, model = newModel, cmd = Cmd.map cfg.wrapMsg cmd, event = Nothing }

        dispatch message =
            Task.perform (always (cfg.wrapMsg message)) (Task.succeed ())

        withEvent toEvent =
            let
                ( uuid, seed ) =
                    Uuid.step appState.seed

                event =
                    toEvent uuid
            in
            { seed = seed, model = applyProjectEvent event model, cmd = Cmd.none, event = Just event }
    in
    case msg of
        ContentMsg contentMsg ->
            let
                result =
                    Content.update appState
                        { wrapMsg = cfg.wrapMsg << ContentMsg
                        , questionnaire = model.questionnaire
                        , knowledgeModelParentMap = model.knowledgeModelParentMap
                        , collapsedPaths = model.collapsedPaths
                        , mbKmEditorUuid = cfg.mbKmEditorUuid
                        , openChapterCmd = dispatch << OpenChapter
                        , scrollToPathCmd = \instant path -> dispatch (ScrollToPath instant path)
                        , updateCollapsedPathsCmd = dispatch << UpdateCollapsedPaths
                        , addTodoCmd = \path -> dispatch (SetLabels path [ ProjectQuestionnaire.todoUuid ])
                        , removeTodoCmd = \path -> dispatch (SetLabels path [])
                        , openFileUploadCmd = \path fileConfig -> dispatch (FileUploadMsg (FileUpload.open path fileConfig))
                        , deleteFileCmd = \fileUuid path fileName -> dispatch (FileDeleteMsg (FileDelete.open fileUuid fileName path))
                        , openCommentsCmd = \path -> Cmd.batch [ dispatch (SetRightPanel (RightPanel.Comments path)), dispatch (ScrollToPath False path) ]
                        , closeRightPanelCmd = dispatch (SetRightPanel RightPanel.None)
                        , openPluginQuestionActionModalCmd = \pluginUuid element question path -> dispatch (OpenQuestionActionModal pluginUuid element question path)
                        , openPluginQuestionActionRightPanelCmd = dispatch << SetRightPanel << RightPanel.PluginQuestionAction
                        }
                        contentMsg
                        model.content

                newModel =
                    { model | content = result.model }
            in
            { seed = result.seed
            , model = Maybe.unwrap newModel (\event -> applyProjectEvent event newModel) result.event
            , cmd = result.cmd
            , event = result.event
            }

        ChapterNavMsg navMsg ->
            -- Choosing somewhere in the navigation closes it when it is an
            -- overlay, so what was chosen is not behind it.
            case navMsg of
                ChapterNav.OpenChapter chapterUuid ->
                    plain (openChapter chapterUuid { model | navOverlayOpen = False })

                ChapterNav.ScrollToPath path ->
                    plain (scrollToPath False path { model | navOverlayOpen = False })

                ChapterNav.ToggleCollapsed path ->
                    plain (setCollapsedPaths (toggle path model.collapsedPaths) model)

        PhaseMsg phaseMsg ->
            case phaseMsg of
                PhaseSelection.OpenModal ->
                    plain ( { model | phaseModalOpen = True }, Cmd.none )

                PhaseSelection.CloseModal ->
                    plain ( { model | phaseModalOpen = False }, Cmd.none )

                PhaseSelection.SetPhase mbPhaseUuid ->
                    update appState cfg (SetPhase mbPhaseUuid) { model | phaseModalOpen = False }

        ToolbarMsg toolbarMsg ->
            let
                ( toolbar, cmd ) =
                    Toolbar.update
                        { wrapMsg = cfg.wrapMsg << ToolbarMsg
                        , setViewSettingsCmd = dispatch << SetViewSettings
                        , openActionCmd = \pluginUuid element -> dispatch (OpenProjectAction pluginUuid element)
                        , togglePanelCmd = dispatch << TogglePanel
                        }
                        appState.locale
                        model.viewSettings
                        toolbarMsg
                        model.toolbar
            in
            { seed = appState.seed, model = { model | toolbar = toolbar }, cmd = cmd, event = Nothing }

        CommentsMsg commentsMsg ->
            let
                result =
                    Comments.update appState { wrapMsg = cfg.wrapMsg << CommentsMsg } commentsMsg model.comments

                commented =
                    { model | comments = result.model }

                ( outcome, outcomeCmd ) =
                    case result.outcome of
                        Just (Comments.OpenComments path) ->
                            openComments appState path commented

                        Just (Comments.ViewResolved resolved) ->
                            ( { commented | viewResolvedComments = resolved }, LocalStorage.storeViewResolved model.uuid resolved )

                        Nothing ->
                            ( commented, Cmd.none )
            in
            { seed = result.seed
            , model = Maybe.unwrap outcome (\event -> applyProjectEvent event outcome) result.event
            , cmd = Cmd.batch [ result.cmd, Cmd.map cfg.wrapMsg outcomeCmd ]
            , event = result.event
            }

        CommentsOverviewMsg overviewMsg ->
            case overviewMsg of
                CommentsOverview.Open path ->
                    plain (openComments appState path model)

                CommentsOverview.ViewResolved resolved ->
                    plain
                        ( { model | viewResolvedComments = resolved }
                        , LocalStorage.storeViewResolved model.uuid resolved
                        )

        SetViewResolved resolved ->
            plain ( { model | viewResolvedComments = resolved }, LocalStorage.storeViewResolved model.uuid resolved )

        GotCommentThreads path result ->
            let
                threads =
                    case ( result, Dict.get path model.commentThreads ) of
                        ( Ok byPath, _ ) ->
                            Success (Maybe.withDefault [] (Dict.get path byPath))

                        -- A refresh that failed keeps the threads already shown.
                        ( Err _, Just (Success shown) ) ->
                            Success shown

                        ( Err _, _ ) ->
                            Error (gettext "Unable to get the comments." appState.locale)

                loaded =
                    { model | commentThreads = Dict.insert path threads model.commentThreads }
            in
            -- A link naming a thread: it decides which tab and which threads are
            -- shown, and is scrolled to once it is on the screen.
            case ( model.commentThreadUuid, threads ) of
                ( Just threadUuid, Success list ) ->
                    case List.find (\thread -> thread.uuid == threadUuid) list of
                        Just thread ->
                            plain
                                ( { loaded
                                    | commentThreadUuid = Nothing
                                    , viewResolvedComments = thread.resolved || model.viewResolvedComments
                                    , comments = Comments.setViewPrivate thread.private loaded.comments
                                  }
                                , Dom.scrollToAndFocus
                                    { selector = "#" ++ Comments.threadElementId threadUuid
                                    , focus = "#" ++ Comments.threadElementId threadUuid
                                    , smooth = False
                                    }
                                )

                        Nothing ->
                            plain ( { loaded | commentThreadUuid = Nothing }, Cmd.none )

                _ ->
                    plain ( loaded, Cmd.none )

        VersionHistoryMsg historyMsg ->
            let
                ( history, historyCmd, outcome ) =
                    VersionHistory.update appState { wrapMsg = cfg.wrapMsg << VersionHistoryMsg } historyMsg model.versionHistory

                withHistory =
                    { model | versionHistory = history }

                ( acted, actedCmd ) =
                    case outcome of
                        Just (VersionHistory.ScrollTo path) ->
                            scrollToPath False path withHistory

                        Just (VersionHistory.NameVersion eventUuid) ->
                            ( { withHistory | versionModal = VersionModal.name eventUuid withHistory.versionModal }, Cmd.none )

                        Just (VersionHistory.RenameVersion version) ->
                            ( { withHistory | versionModal = VersionModal.rename version withHistory.versionModal }, Cmd.none )

                        Just (VersionHistory.DeleteVersion version) ->
                            ( { withHistory | deleteVersionModal = DeleteVersionModal.open version withHistory.deleteVersionModal }, Cmd.none )

                        Just (VersionHistory.NamedOnlyChanged namedOnly) ->
                            ( { withHistory | viewNamedOnlyVersions = namedOnly }
                            , LocalStorage.storeNamedOnly model.uuid namedOnly
                            )

                        Just (VersionHistory.ViewVersion _ _) ->
                            ( withHistory, Cmd.none )

                        Just (VersionHistory.Revert _) ->
                            ( withHistory, Cmd.none )

                        Nothing ->
                            ( withHistory, Cmd.none )

                -- Viewing a version and reverting belong to the page around the
                -- questionnaire; it says whether it can do them (`ViewConfig`).
                pageCmd =
                    case ( outcome, cfg.previewVersionMsg, cfg.revertVersionMsg ) of
                        ( Just (VersionHistory.ViewVersion eventUuid title), Just toMsg, _ ) ->
                            dispatchMsg (toMsg eventUuid title)

                        ( Just (VersionHistory.Revert event), _, Just toMsg ) ->
                            dispatchMsg (toMsg event)

                        _ ->
                            Cmd.none
            in
            { seed = appState.seed
            , model = acted
            , cmd = Cmd.batch [ historyCmd, Cmd.map cfg.wrapMsg actedCmd, pageCmd ]
            , event = Nothing
            }

        VersionModalMsg modalMsg ->
            let
                ( versionModal, modalCmd ) =
                    VersionModal.update appState
                        { wrapMsg = cfg.wrapMsg << VersionModalMsg
                        , savedMsg = cfg.wrapMsg << VersionSaved
                        }
                        modalMsg
                        model.versionModal
            in
            { seed = appState.seed, model = { model | versionModal = versionModal }, cmd = modalCmd, event = Nothing }

        DeleteVersionModalMsg modalMsg ->
            let
                ( deleteVersionModal, modalCmd ) =
                    DeleteVersionModal.update appState
                        { wrapMsg = cfg.wrapMsg << DeleteVersionModalMsg
                        , deletedMsg = cfg.wrapMsg << VersionDeleted
                        }
                        modalMsg
                        model.deleteVersionModal
            in
            { seed = appState.seed, model = { model | deleteVersionModal = deleteVersionModal }, cmd = modalCmd, event = Nothing }

        VersionSaved version ->
            plain ( { model | versionHistory = VersionHistory.setVersion version model.versionHistory }, Cmd.none )

        VersionDeleted version ->
            plain ( { model | versionHistory = VersionHistory.removeVersion version model.versionHistory }, Cmd.none )

        SearchMsg searchMsg ->
            let
                ( search, searchCmd, outcome ) =
                    Search.update appState.locale model.questionnaire searchMsg model.search

                found =
                    { model | search = search }

                -- A result is a place: a reply's own path, a question that may
                -- be folded away or in another chapter, or a whole chapter.
                ( moved, movedCmd ) =
                    case outcome of
                        Just (Search.GoToPath path) ->
                            scrollToPath False path found

                        Just (Search.GoToQuestion questionUuid) ->
                            case ProjectQuestionnaire.getClosestQuestionParentPath model.questionnaire model.knowledgeModelParentMap questionUuid of
                                Just path ->
                                    scrollToPath False path found

                                Nothing ->
                                    ( found, Cmd.none )

                        Just (Search.GoToChapter chapterUuid) ->
                            openChapter chapterUuid found

                        Nothing ->
                            ( found, Cmd.none )
            in
            plain ( moved, Cmd.batch [ Cmd.map SearchMsg searchCmd, movedCmd ] )

        OpenChapter chapterUuid ->
            plain (openChapter chapterUuid model)

        OpenCommentsLink path mbThreadUuid ->
            let
                ( opened, openCmd ) =
                    setRightPanel appState (RightPanel.Comments path) { model | commentThreadUuid = mbThreadUuid }

                ( scrolled, scrollCmd ) =
                    -- Instant: the reader followed a link to this question and
                    -- did not ask to watch the page travel to it.
                    scrollToPath True path opened
            in
            plain ( scrolled, Cmd.batch [ openCmd, scrollCmd ] )

        ScrollToPath instant path ->
            plain (scrollToPath instant path model)

        ClearHighlightedPath path ->
            if model.viewState.highlightedPath == Just path then
                plain ( setViewState (\state -> { state | highlightedPath = Nothing }) model, Cmd.none )

            else
                plain ( model, Cmd.none )

        UpdateCollapsedPaths collapsedPaths ->
            plain (setCollapsedPaths collapsedPaths model)

        SetViewSettings viewSettings ->
            plain
                ( rebuild { model | viewSettings = viewSettings }
                , LocalStorage.storeViewSettings viewSettings
                )

        SetRightPanel panel ->
            plain (setRightPanel appState panel model)

        TogglePanel panel ->
            let
                open =
                    case panel of
                        RightPanel.CommentsOverview ->
                            model.rightPanel == RightPanel.CommentsOverview

                        _ ->
                            model.rightPanel == panel
            in
            plain
                (setRightPanel appState
                    (if open then
                        RightPanel.None

                     else
                        panel
                    )
                    model
                )

        SetNavOverlay open ->
            plain
                ( { model | navOverlayOpen = open }
                , Dom.focus
                    ("#"
                        ++ (if open then
                                Workspace.navId (workspaceId model)

                            else
                                Workspace.navToggleId (workspaceId model)
                           )
                    )
                )

        NavResizerMsg resizerMsg ->
            plain ( { model | navResizer = Resizer.update resizerMsg model.navResizer }, Cmd.none )

        AsideResizerMsg resizerMsg ->
            plain ( { model | asideResizer = Resizer.update resizerMsg model.asideResizer }, Cmd.none )

        LocalStorageGot value ->
            case LocalStorage.decode model.uuid value of
                Just (LocalStorage.ViewSettingsStored viewSettings) ->
                    plain ( rebuild { model | viewSettings = viewSettings }, Cmd.none )

                Just (LocalStorage.CollapsedPathsStored collapsedPaths) ->
                    -- A deep link has expanded its way to its question already;
                    -- what was collapsed last time must not fold it away again.
                    let
                        kept =
                            case model.viewState.highlightedPath of
                                Just path ->
                                    List.foldl Set.remove collapsedPaths (pathPrefixes path)

                                Nothing ->
                                    collapsedPaths
                    in
                    plain ( rebuild { model | collapsedPaths = kept }, Cmd.none )

                Just (LocalStorage.RightPanelStored panel) ->
                    plain (setRightPanel appState panel model)

                Just (LocalStorage.ViewResolvedStored viewResolved) ->
                    plain ( { model | viewResolvedComments = viewResolved }, Cmd.none )

                Just (LocalStorage.NamedOnlyStored namedOnly) ->
                    plain ( { model | viewNamedOnlyVersions = namedOnly }, Cmd.none )

                Nothing ->
                    plain ( model, Cmd.none )

        GotScrollTop scrollTop ->
            if scrollTop.selector == mainSelector model && Just scrollTop.scrollTop /= model.contentScrollTop then
                plain ( { model | contentScrollTop = Just scrollTop.scrollTop }, Cmd.none )

            else
                plain ( model, Cmd.none )

        RestoreScroll ->
            plain
                ( model
                , Cmd.batch
                    [ Dom.watchScrollTop (mainSelector model)
                    , case model.contentScrollTop of
                        Just scrollTop ->
                            Dom.setScrollTop { selector = mainSelector model, scrollTop = scrollTop }

                        Nothing ->
                            Cmd.none
                    ]
                )

        SetPhase mbPhaseUuid ->
            withEvent
                (\uuid ->
                    ProjectEvent.SetPhase
                        { uuid = uuid
                        , phaseUuid = mbPhaseUuid
                        , createdAt = appState.currentTime
                        , createdBy = Maybe.map UserConfig.toUserSuggestion appState.config.user
                        }
                )

        SetLabels path labels ->
            withEvent
                (\uuid ->
                    ProjectEvent.SetLabels
                        { uuid = uuid
                        , path = path
                        , value = labels
                        , createdAt = appState.currentTime
                        , createdBy = Maybe.map UserConfig.toUserSuggestion appState.config.user
                        }
                )

        FileUploadMsg uploadMsg ->
            let
                ( fileUpload, cmd ) =
                    FileUpload.update appState
                        { wrapMsg = cfg.wrapMsg << FileUploadMsg
                        , kmEditor = Maybe.isJust cfg.mbKmEditorUuid
                        , setFileMsg = \path file -> cfg.wrapMsg (SetFile path file)
                        }
                        uploadMsg
                        model.fileUpload
            in
            { seed = appState.seed, model = { model | fileUpload = fileUpload }, cmd = cmd, event = Nothing }

        FileDeleteMsg deleteMsg ->
            let
                ( fileDelete, cmd ) =
                    FileDelete.update appState
                        { wrapMsg = cfg.wrapMsg << FileDeleteMsg
                        , deleteFileMsg = cfg.wrapMsg << RemoveFile
                        }
                        deleteMsg
                        model.fileDelete
            in
            { seed = appState.seed, model = { model | fileDelete = fileDelete }, cmd = cmd, event = Nothing }

        SetFile path file ->
            let
                ( uuid, seed ) =
                    Uuid.step appState.seed

                event =
                    ProjectEvent.SetReply
                        { uuid = uuid
                        , path = path
                        , value = ReplyValue.FileReply file.uuid
                        , createdAt = appState.currentTime
                        , createdBy = Maybe.map UserConfig.toUserSuggestion appState.config.user
                        }
            in
            { seed = seed, model = applyProjectEvent event (addFile file model), cmd = Cmd.none, event = Just event }

        RemoveFile path ->
            withEvent
                (\uuid ->
                    ProjectEvent.ClearReply
                        { uuid = uuid
                        , path = path
                        , createdAt = appState.currentTime
                        , createdBy = Maybe.map UserConfig.toUserSuggestion appState.config.user
                        }
                )

        OpenProjectAction pluginUuid element ->
            plain
                ( { model
                    | projectActionModal =
                        PluginModal.update
                            (PluginModal.open { pluginUuid = pluginUuid, pluginElement = element, data = cfg.projectCommon })
                            model.projectActionModal
                  }
                , Cmd.none
                )

        ProjectActionModalMsg modalMsg ->
            plain ( { model | projectActionModal = PluginModal.update modalMsg model.projectActionModal }, Cmd.none )

        OpenQuestionActionModal pluginUuid element question path ->
            plain
                ( { model
                    | questionActionModal =
                        PluginModal.update
                            (PluginModal.open { pluginUuid = pluginUuid, pluginElement = element, data = ( cfg.projectCommon, question, path ) })
                            model.questionActionModal
                  }
                , Cmd.none
                )

        QuestionActionModalMsg modalMsg ->
            plain ( { model | questionActionModal = PluginModal.update modalMsg model.questionActionModal }, Cmd.none )


toggle : comparable -> Set comparable -> Set comparable
toggle value set =
    if Set.member value set then
        Set.remove value set

    else
        Set.insert value set


setCollapsedPaths : Set String -> Model -> ( Model, Cmd Msg )
setCollapsedPaths collapsedPaths model =
    ( rebuild { model | collapsedPaths = collapsedPaths }
    , LocalStorage.storeCollapsedPaths model.uuid collapsedPaths
    )


{-| Another chapter, from its start, with focus on its heading.
-}
openChapter : String -> Model -> ( Model, Cmd Msg )
openChapter chapterUuid model =
    ( rebuild { model | chapterUuid = chapterUuid }
    , Dom.scrollToAndFocus
        { selector = focusSelector chapterUuid
        , focus = focusSelector chapterUuid
        , smooth = False
        }
    )


{-| The value only when the condition holds.
-}
maybeWhen : Bool -> Maybe a -> Maybe a
maybeWhen condition value =
    if condition then
        value

    else
        Nothing


{-| A message on its way back into the application's own update.
-}
dispatchMsg : msg -> Cmd msg
dispatchMsg msg =
    Task.perform (always msg) (Task.succeed ())


{-| A question's comments: the panel opens on it (which asks for its threads)
and the question is scrolled to.
-}
openComments : AppState -> String -> Model -> ( Model, Cmd Msg )
openComments appState path model =
    let
        ( opened, openCmd ) =
            setRightPanel appState (RightPanel.Comments path) model

        ( scrolled, scrollCmd ) =
            scrollToPath False path opened
    in
    ( scrolled, Cmd.batch [ openCmd, scrollCmd ] )


{-| The threads of a question, asked for each time its panel opens: those already
here are shown meanwhile, and what the websocket did not bring (a reconnect, a
missed event) is put right.
-}
fetchThreads : AppState -> String -> Model -> Cmd Msg
fetchThreads appState path model =
    ProjectsApi.getCommentThreads appState model.uuid path (GotCommentThreads path)


{-| The panel beside the questionnaire, as far as the project allows it: TODOs,
comments and the version history are features a project can do without.
-}
setRightPanel : AppState -> RightPanel -> Model -> ( Model, Cmd Msg )
setRightPanel appState requested model =
    let
        allowed feature =
            if feature appState model.questionnaire then
                requested

            else
                RightPanel.None

        panel =
            case requested of
                RightPanel.TODOs ->
                    allowed Feature.projectTodos

                RightPanel.VersionHistory ->
                    allowed Feature.projectVersionHistory

                RightPanel.CommentsOverview ->
                    allowed Feature.projectCommentAdd

                RightPanel.Comments _ ->
                    allowed Feature.projectCommentAdd

                _ ->
                    requested

        -- The plugin's panel is marked on its own question only, with a value
        -- held here, so that one node re-renders.
        openActions =
            case panel of
                RightPanel.PluginQuestionAction data ->
                    Just
                        ( data.questionPath
                        , { plugins = model.content.questionActions.plugins
                          , open = Just ( data.plugin.uuid, data.connector )
                          }
                        )

                _ ->
                    Nothing

        commentsOpenPath =
            RightPanel.commentsPath panel

        -- Most panels mark no question: the content's state is kept as it is,
        -- so its top-level lazy call hits and opening a panel renders the panel.
        withViewState =
            if commentsOpenPath == model.viewState.commentsOpenPath && Maybe.isNothing openActions && Maybe.isNothing model.viewState.openActions then
                identity

            else
                setViewState (\state -> { state | commentsOpenPath = commentsOpenPath, openActions = openActions })
    in
    ( withViewState
        { model
            | rightPanel = panel
            , unanswered = keepUnanswered panel model
            , commentInfos = keepCommentInfos panel model
        }
    , Cmd.batch
        [ LocalStorage.storeRightPanel model.uuid panel
        , case panel of
            RightPanel.Comments path ->
                fetchThreads appState path model

            RightPanel.VersionHistory ->
                if model.rightPanel == RightPanel.VersionHistory then
                    Cmd.none

                else
                    dispatchMsg (VersionHistoryMsg VersionHistory.initMsg)

            _ ->
                Cmd.none
        , -- The search panel is opened to type in it, from the toolbar or with
          -- Ctrl/Cmd+F, so the cursor goes there.
          if panel == RightPanel.Search && model.rightPanel /= RightPanel.Search then
            Dom.focus ("#" ++ Search.inputId)

          else
            Cmd.none
        ]
    )


{-| Where the comments are, while the overview is open: the list is a walk of the
whole questionnaire, like the unanswered questions.
-}
keepCommentInfos : RightPanel -> Model -> Maybe (List QuestionCommentInfo)
keepCommentInfos panel model =
    case ( panel, model.commentInfos ) of
        ( RightPanel.CommentsOverview, Just infos ) ->
            Just infos

        ( RightPanel.CommentsOverview, Nothing ) ->
            Just (ProjectQuestionnaire.getComments model.questionnaire)

        _ ->
            Nothing


{-| The unanswered questions while their panel is open, and nothing otherwise:
the list is worked out when the panel opens and given up when it closes.
-}
keepUnanswered : RightPanel -> Model -> Maybe (List QuestionnaireUnansweredQuestion)
keepUnanswered panel model =
    case ( panel, model.unanswered ) of
        ( RightPanel.UnansweredQuestions, Just unanswered ) ->
            Just unanswered

        ( RightPanel.UnansweredQuestions, Nothing ) ->
            Just (ProjectQuestionnaire.getUnansweredQuestions model.questionnaire)

        _ ->
            Nothing


{-| A path and every path above it, the chapter first.
-}
pathPrefixes : String -> List String
pathPrefixes path =
    String.split "." path
        |> List.inits
        |> List.drop 1
        |> List.map (String.join ".")


{-| What names the node at `path` — a chapter's heading, or a question's or an
item's title — which is what is scrolled to and focused. By id, because the
lookup runs over the whole page, and an attribute selector over a chapter of
thousands of elements is a search where an id is a lookup.
-}
focusSelector : String -> String
focusSelector path =
    if String.contains "." path then
        "#" ++ Nest.titleId path

    else
        "#" ++ Content.chapterId path


{-| A file someone uploaded, from the websocket or after an upload here, so the
file question that is answered with it can show it.
-}
addFile : ProjectFileSimple -> Model -> Model
addFile file model =
    let
        questionnaire =
            ProjectQuestionnaire.addFile file model.questionnaire
    in
    { model | warnings = ProjectQuestionnaire.getWarnings questionnaire }
        |> setQuestionnaire questionnaire


{-| The project's files as the server has them now. A file deleted from the files
tab goes without an event, so the questions answered with it learn it is gone
when the questionnaire is fetched again.
-}
setFiles : List ProjectFileSimple -> Model -> Model
setFiles files model =
    let
        questionnaire =
            model.questionnaire

        withFiles =
            { questionnaire | files = files }
    in
    { model | warnings = ProjectQuestionnaire.getWarnings withFiles }
        |> setQuestionnaire withFiles


{-| Whether anything has been uploaded to the questionnaire, here or by
somebody else since it was loaded.
-}
hasFiles : Model -> Bool
hasFiles model =
    not (List.isEmpty model.questionnaire.files)


{-| The project's data that changed elsewhere — its name, sharing, labels,
comment counts — from the websocket.
-}
updateWithQuestionnaireData : SetProjectData -> Model -> Model
updateWithQuestionnaireData data model =
    let
        questionnaire =
            ProjectQuestionnaire.updateWithQuestionnaireData data model.questionnaire
    in
    { model | commentsCount = ProjectQuestionnaire.commentsLength questionnaire }
        |> setQuestionnaire questionnaire


{-| The project's sharing after the share dialog saved it.
-}
updateWithShareData : { a | permissions : List Permission, sharing : ProjectSharing, visibility : ProjectVisibility } -> Model -> Model
updateWithShareData data model =
    setQuestionnaire (ProjectQuestionnaire.updateWithShareData data model.questionnaire) model


{-| Go to `path`: its chapter, with everything above it expanded, highlighted for a
moment, scrolled into view and focused.
-}
scrollToPath : Bool -> String -> Model -> ( Model, Cmd Msg )
scrollToPath instant path model =
    let
        chapterUuid =
            Path.chapterUuid path

        collapsedPaths =
            List.foldl Set.remove model.collapsedPaths (pathPrefixes path)

        needsRebuild =
            chapterUuid /= model.chapterUuid || collapsedPaths /= model.collapsedPaths

        moved =
            { model | chapterUuid = chapterUuid, collapsedPaths = collapsedPaths }
                |> (if needsRebuild then
                        rebuild

                    else
                        identity
                   )
                |> setViewState (\state -> { state | highlightedPath = Just path })
    in
    ( moved
    , Cmd.batch
        [ Dom.scrollToAndFocus
            { selector = focusSelector path
            , focus = focusSelector path
            , smooth = not instant
            }
        , if collapsedPaths /= model.collapsedPaths then
            LocalStorage.storeCollapsedPaths model.uuid collapsedPaths

          else
            Cmd.none
        , Task.perform (always (ClearHighlightedPath path)) (Process.sleep 3000)
        ]
    )


{-| Apply an event to the questionnaire — one the user made (`update` has already
done this), or one from the websocket or an importer — and work out again only
what it can have changed.
-}
applyProjectEvent : ProjectEvent -> Model -> Model
applyProjectEvent event model =
    let
        before =
            model.questionnaire

        after =
            case event of
                ProjectEvent.SetReply data ->
                    ProjectQuestionnaire.setReply data.path (SetReplyData.toReply data) before

                ProjectEvent.ClearReply data ->
                    ProjectQuestionnaire.clearReplyValue data.path before

                ProjectEvent.SetPhase data ->
                    ProjectQuestionnaire.setPhaseUuid data.phaseUuid before

                ProjectEvent.SetLabels data ->
                    ProjectQuestionnaire.setLabels data.path data.value before

                ProjectEvent.AddComment data ->
                    ProjectQuestionnaire.addCommentCount data.path data.threadUuid before

                ProjectEvent.DeleteComment data ->
                    ProjectQuestionnaire.subCommentCount data.path data.threadUuid before

                ProjectEvent.ResolveCommentThread data ->
                    ProjectQuestionnaire.addResolvedCommentThreadToCount data.path data.threadUuid data.commentCount before

                ProjectEvent.ReopenCommentThread data ->
                    ProjectQuestionnaire.addReopenedCommentThreadToCount data.path data.threadUuid data.commentCount before

                ProjectEvent.DeleteCommentThread data ->
                    ProjectQuestionnaire.removeCommentThreadFromCount data.path data.threadUuid before

                -- Editing a comment or assigning a thread moves no count.
                _ ->
                    before

        counted =
            { model
                | versionHistory = VersionHistory.addEvent event model.versionHistory
                , commentThreads = applyToThreads event model.commentThreads
                , commentInfos = Maybe.map (\_ -> ProjectQuestionnaire.getComments after) model.commentInfos
                , chapterUnansweredCounts = Rebuild.updateChapterUnansweredCounts event { before = before, after = after } model.chapterUnansweredCounts
                , todos = Rebuild.updateTodos event after model.todos
                , warnings = Rebuild.updateWarnings event after model.warnings
                , unanswered = Rebuild.updateUnanswered event after model.unanswered
                , commentsCount = Rebuild.updateCommentsCount event after model.commentsCount
            }
                |> setQuestionnaire after
    in
    case event of
        ProjectEvent.SetPhase _ ->
            rebuild counted

        _ ->
            if Rebuild.needsRebuild event then
                rebuild counted

            else
                -- An item's title is the answer to its first question: the
                -- navigation shows it, and is the same value if it did not change.
                { counted | nav = ChapterNav.refreshItemTitles after counted.nav }


{-| A comment event on the threads that are here: the user's own, applied as it
is made, and somebody else's, applied as it arrives.
-}
applyToThreads : ProjectEvent -> Dict String (ActionResult (List CommentThread)) -> Dict String (ActionResult (List CommentThread))
applyToThreads event threads =
    let
        onPath path change =
            Dict.update path (Maybe.map (ActionResult.map change)) threads

        onThread path threadUuid change =
            onPath path (List.map (\thread -> ifThread threadUuid change thread))

        ifThread threadUuid change thread =
            if thread.uuid == threadUuid then
                change thread

            else
                thread
    in
    case event of
        ProjectEvent.AddComment data ->
            onPath data.path
                (\list ->
                    let
                        comment =
                            { uuid = data.commentUuid
                            , text = data.text
                            , createdBy = data.createdBy
                            , createdAt = data.createdAt
                            , updatedAt = data.createdAt
                            }
                    in
                    -- A comment on a thread that is not here starts it: the server's
                    -- event does not say `newThread`, so somebody else's first
                    -- comment is known only by its thread being new.
                    if not (List.any (\thread -> thread.uuid == data.threadUuid) list) then
                        list
                            ++ [ { uuid = data.threadUuid
                                 , resolved = False
                                 , comments = [ comment ]
                                 , private = data.private
                                 , createdAt = data.createdAt
                                 , createdBy = data.createdBy
                                 , assignedTo = Nothing
                                 }
                               ]

                    else
                        List.map (ifThread data.threadUuid (\thread -> { thread | comments = thread.comments ++ [ comment ] })) list
                )

        ProjectEvent.EditComment data ->
            onThread data.path
                data.threadUuid
                (\thread ->
                    { thread
                        | comments =
                            List.map
                                (\comment ->
                                    if comment.uuid == data.commentUuid then
                                        { comment | text = data.text, updatedAt = data.createdAt }

                                    else
                                        comment
                                )
                                thread.comments
                    }
                )

        ProjectEvent.DeleteComment data ->
            onThread data.path data.threadUuid (\thread -> { thread | comments = List.filter (\comment -> comment.uuid /= data.commentUuid) thread.comments })

        ProjectEvent.ResolveCommentThread data ->
            onThread data.path data.threadUuid (\thread -> { thread | resolved = True })

        ProjectEvent.ReopenCommentThread data ->
            onThread data.path data.threadUuid (\thread -> { thread | resolved = False })

        ProjectEvent.DeleteCommentThread data ->
            onPath data.path (List.filter (\thread -> thread.uuid /= data.threadUuid))

        ProjectEvent.AssignCommentThread data ->
            onThread data.path data.threadUuid (\thread -> { thread | assignedTo = data.assignedTo })

        _ ->
            threads


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ToolbarMsg (Toolbar.subscriptions model.toolbar)
        , Sub.map CommentsMsg (Comments.subscriptions model.comments)
        , Sub.map VersionHistoryMsg (VersionHistory.subscriptions model.versionHistory)
        , Sub.map NavResizerMsg (Resizer.subscriptions model.navResizer)
        , Sub.map AsideResizerMsg (Resizer.subscriptions model.asideResizer)
        , Sub.map ContentMsg (Content.subscriptions model.content)
        , LocalStoragePort.gotItemRaw LocalStorageGot
        , Dom.gotScrollTop GotScrollTop
        ]



-- VIEW


type alias ViewConfig msg =
    { wrapMsg : Msg -> msg
    , readonly : Bool

    -- Apart from `readonly`: a commenter cannot change the answers but can
    -- comment on them.
    , commentsReadonly : Bool
    , actionsEnabled : Bool
    , toolbarEnabled : Bool

    -- What the page around the questionnaire offers; it says how in its
    -- `UpdateConfig`, and here only whether the history should offer it.
    , canViewVersions : Bool
    , canRevert : Bool
    }


view : AppState -> ViewConfig msg -> Model -> Html msg
view appState cfg model =
    let
        questionnaire =
            model.questionnaire

        flags =
            Flags.base
                { commentsEnabled = Feature.projectCommentAdd appState questionnaire
                , readOnly = cfg.readonly
                , showActions = cfg.actionsEnabled
                }
                model.viewSettings

        -- The time the "answered … ago" texts are measured from, to the minute:
        -- the app refreshes its clock after every message, and a value that
        -- changed each time would make the lazy call below miss each time.
        minute =
            (Time.posixToMillis appState.currentTime // 60000) * 60000

        content =
            Html.map (cfg.wrapMsg << ContentMsg)
                (Lazy.lazy6 Content.view appState.locale appState.timeZone minute flags model.viewState model.content)

        -- The phase and the list's title stay; only the chapters scroll.
        navHeader =
            [ Html.map (cfg.wrapMsg << PhaseMsg)
                (PhaseSelection.viewBox (model.idPrefix ++ "-phase-label") appState.locale model.kmLocale questionnaire.knowledgeModel cfg.readonly questionnaire.phaseUuid (ProjectQuestionnaire.getCurrentPhaseIndex questionnaire))
            , Html.span [ class "c-questionnaire-nav__title" ] [ Html.text (gettext "Chapters" appState.locale) ]
            ]

        navContent =
            [ Html.map (cfg.wrapMsg << ChapterNavMsg)
                (Lazy.lazy3 ChapterNav.view appState.locale model.nav model.chapterUnansweredCounts)
            ]

        withToolbar =
            if cfg.toolbarEnabled then
                Workspace.withToolbar
                    { label = gettext "Questionnaire tools" appState.locale
                    , start = [ Html.map (cfg.wrapMsg << ToolbarMsg) (Toolbar.viewStart appState.locale (Routes.toUrl appState (Routes.projectsImport model.questionnaire.uuid Toolbar.importerPlaceholder)) model.viewSettings (Feature.projectToolbarImporters appState questionnaire) model.toolbar) ]
                    , end =
                        [ Html.map (cfg.wrapMsg << ToolbarMsg)
                            (Toolbar.viewEnd appState.locale
                                model.rightPanel
                                { warnings = List.length model.warnings
                                , unanswered = List.sum (Dict.values model.chapterUnansweredCounts)
                                , todos = List.length model.todos
                                , comments = model.commentsCount
                                }
                                (toolbarFeatures appState questionnaire)
                            )
                        ]
                    }

            else
                identity

        -- A panel is opened and closed from the toolbar, so a questionnaire
        -- shown without one — a preview, a version as it was — has none: it
        -- would be a panel nothing could close.
        withAside =
            case maybeWhen cfg.toolbarEnabled (viewPanel appState cfg model) of
                Just ( title, panelContent ) ->
                    Workspace.withAside
                        { title = title
                        , content = panelContent
                        , resizer = model.asideResizer
                        , toResizerMsg = cfg.wrapMsg << AsideResizerMsg
                        , resizerWords = UiWords.resizer appState (gettext "Resize panel" appState.locale)
                        , onClose = cfg.wrapMsg (SetRightPanel RightPanel.None)
                        , closeLabel = gettext "Close panel" appState.locale
                        , closeInPanel = False
                        }

                Nothing ->
                    identity

        workspace =
            Workspace.workspace { id = workspaceId model, main = [ content ] }
                |> Workspace.withNav
                    { label = gettext "Chapters" appState.locale
                    , header = navHeader
                    , content = navContent
                    , resizer = model.navResizer
                    , toResizerMsg = cfg.wrapMsg << NavResizerMsg
                    , resizerWords = UiWords.resizer appState (gettext "Resize chapters" appState.locale)
                    , overlayOpen = model.navOverlayOpen
                    , onOverlay = cfg.wrapMsg << SetNavOverlay
                    , toggleLabel = gettext "Chapters" appState.locale
                    , closeLabel = gettext "Close chapters" appState.locale
                    }
                |> withToolbar
                |> withAside

        shortcuts =
            Shortcut.shortcutElement
                (Shortcut.metaShortcut (Shortcut.Regular "f") (cfg.wrapMsg (SetRightPanel RightPanel.Search))
                    :: Shortcut.ctrlShortcut (Shortcut.Regular "f") (cfg.wrapMsg (SetRightPanel RightPanel.Search))
                    :: (if model.rightPanel /= RightPanel.None then
                            [ Shortcut.esc (cfg.wrapMsg (SetRightPanel RightPanel.None)) ]

                        else
                            []
                       )
                )
                -- It listens on the document and needs no box.
                [ style "display" "none" ]
                []
    in
    div [ class "c-questionnaire", attribute "data-tour" "questionnaire_body" ]
        (Workspace.toHtml workspace
            :: shortcuts
            :: dialogs appState cfg model
        )


{-| The dialogs a questionnaire's own actions open. A read-only one has no
actions, so it has no dialogs either — and it is the one shown over another
questionnaire, where a second copy of each would be a second element with the
same id.
-}
dialogs : AppState -> ViewConfig msg -> Model -> List (Html msg)
dialogs appState cfg model =
    if cfg.readonly then
        []

    else
        let
            questionnaire =
                model.questionnaire
        in
        [ Html.map (cfg.wrapMsg << FileUploadMsg) (FileUpload.view appState model.fileUpload)
        , Html.map (cfg.wrapMsg << FileDeleteMsg) (FileDelete.view appState model.fileDelete)
        , Html.map (cfg.wrapMsg << VersionModalMsg) (VersionModal.view appState model.versionModal)
        , Html.map (cfg.wrapMsg << DeleteVersionModalMsg) (DeleteVersionModal.view appState model.deleteVersionModal)
        , Html.map (cfg.wrapMsg << PhaseMsg)
            (PhaseSelection.viewModal appState.locale model.kmLocale questionnaire.knowledgeModel (ProjectQuestionnaire.getCurrentPhaseIndex questionnaire) model.phaseModalOpen)
        , PluginModal.viewWithId "questionnaire-project-action"
            appState
            { attributes = \project -> [ PluginElement.projectValue project ]
            , wrapMsg = cfg.wrapMsg << ProjectActionModalMsg
            }
            model.projectActionModal
        , PluginModal.viewWithId "questionnaire-question-action"
            appState
            { attributes =
                \( project, question, path ) ->
                    [ PluginElement.projectValue project
                    , PluginElement.questionValue question
                    , PluginElement.questionPathValue path
                    ]
            , wrapMsg = cfg.wrapMsg << QuestionActionModalMsg
            }
            model.questionActionModal
        ]


{-| Which of the optional panels the project offers, as the toolbar's bitmask.
-}
toolbarFeatures : AppState -> ProjectQuestionnaire -> Int
toolbarFeatures appState questionnaire =
    let
        bit feature value =
            if feature appState questionnaire then
                value

            else
                0
    in
    bit Feature.projectTodos 1 + bit Feature.projectCommentAdd 2 + bit Feature.projectVersionHistory 4


{-| The open panel's title and content. The panels themselves arrive with
milestone Q5; a plugin's panel is here already.
-}
viewPanel : AppState -> ViewConfig msg -> Model -> Maybe ( String, List (Html msg) )
viewPanel appState cfg model =
    let
        -- A row of a list panel is a place in the questionnaire: choosing it
        -- goes there, as the navigation does.
        list title panel =
            Just ( title, [ Html.map (\(Group.Selected path) -> cfg.wrapMsg (ScrollToPath False path)) panel ] )
    in
    case model.rightPanel of
        RightPanel.None ->
            Nothing

        RightPanel.Search ->
            Just
                ( gettext "Search" appState.locale
                , [ Html.map (cfg.wrapMsg << SearchMsg) (Search.view appState.locale model.search) ]
                )

        RightPanel.UnansweredQuestions ->
            list (gettext "Unanswered questions" appState.locale)
                (Lists.viewUnanswered appState.locale (Maybe.withDefault [] model.unanswered))

        RightPanel.TODOs ->
            list (gettext "TODOs" appState.locale) (Lists.viewTodos appState.locale model.todos)

        RightPanel.VersionHistory ->
            Just
                ( gettext "Version history" appState.locale
                , [ Html.map (cfg.wrapMsg << VersionHistoryMsg)
                        (VersionHistory.view appState
                            { questionnaire = model.questionnaire
                            , namedOnly = model.viewNamedOnlyVersions
                            , canView = cfg.canViewVersions
                            , canRevert = cfg.canRevert
                            }
                            model.versionHistory
                        )
                  ]
                )

        RightPanel.CommentsOverview ->
            Just
                ( gettext "Comments" appState.locale
                , [ Html.map (cfg.wrapMsg << CommentsOverviewMsg)
                        (CommentsOverview.view appState.locale model.viewResolvedComments (Maybe.withDefault [] model.commentInfos))
                  ]
                )

        RightPanel.Comments path ->
            let
                commented =
                    ProjectQuestionnaire.getComments model.questionnaire
                        |> List.filter (\comment -> comment.unresolvedComments > 0 || comment.resolvedComments > 0)
                        |> List.map .path

                index =
                    List.elemIndex path commented
            in
            Just
                ( gettext "Comments" appState.locale
                , [ Html.map (cfg.wrapMsg << CommentsMsg)
                        (Comments.view appState
                            { path = path
                            , threads = Maybe.withDefault Loading (Dict.get path model.commentThreads)
                            , questionnaire = model.questionnaire
                            , viewResolved = model.viewResolvedComments
                            , readonly = cfg.commentsReadonly
                            , previousPath = Maybe.andThen (\i -> List.getAt (i - 1) commented) index
                            , nextPath = Maybe.andThen (\i -> List.getAt (i + 1) commented) index
                            , position = Maybe.map (\i -> ( i + 1, List.length commented )) index
                            }
                            model.comments
                        )
                  ]
                )

        RightPanel.Warnings ->
            list (gettext "Warnings" appState.locale) (Lists.viewWarnings appState.locale model.warnings)

        RightPanel.PluginQuestionAction data ->
            Just
                ( data.connector.action.name
                , [ Html.Keyed.node "div"
                        [ class "c-questionnaire-plugin-panel" ]
                        [ ( data.questionPath
                          , PluginView.view appState
                                data.plugin.uuid
                                data.connector.element
                                [ PluginElement.projectValue (ProjectQuestionnaire.toProjectCommon model.questionnaire)
                                , PluginElement.questionValue data.question
                                , PluginElement.questionPathValue data.questionPath
                                , PluginElement.onActionClose (cfg.wrapMsg (SetRightPanel RightPanel.None))
                                ]
                          )
                        ]
                  ]
                )


resourcePagePlaceholder : String
resourcePagePlaceholder =
    "__resource-page__"
