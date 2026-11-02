module Common.Pages.KMEditor.Editor exposing
    ( Model
    , Msg
    , close
    , entityName
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

{-| The knowledge model editor: a tree of everything in a knowledge model, a
form for whatever is selected, and a websocket carrying every change to and
from everybody else in it.

**The protocol is the server's, exactly.** An edit becomes
an event; the event is applied to the local store at once and sent over the
socket; the server broadcasts it back to everybody including its author. An
arriving action uuid that is on `savingActionUuids` is my own echo and is
dropped; one that is not is somebody else's and is applied with `local = False`,
which is what keeps a colleague's edit from moving my cursor.

**Typing is debounced per entity**, squashed onto the last event for that
entity, so a title typed a letter at a time is one event on the wire. Structural
changes — add, delete, move, a change of type — go immediately, because a
reorder squashed onto the edit before it would be a different knowledge model.

**The active entity is in the URL.** Selecting in the tree is a navigation, the
back button walks the editing history, and a link to an entity can be shared.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Navigation as Nav
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Common.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Common.Api.KnowledgeModelSecrets as KnowledgeModelSecretsApi
import Common.Api.Models.Event as Event exposing (Event)
import Common.Api.Models.Event.AddAnswerEventData as AddAnswerEventData
import Common.Api.Models.Event.AddChapterEventData as AddChapterEventData
import Common.Api.Models.Event.AddChoiceEventData as AddChoiceEventData
import Common.Api.Models.Event.AddExpertEventData as AddExpertEventData
import Common.Api.Models.Event.AddIntegrationEventData as AddIntegrationEventData
import Common.Api.Models.Event.AddMetricEventData as AddMetricEventData
import Common.Api.Models.Event.AddPhaseEventData as AddPhaseEventData
import Common.Api.Models.Event.AddQuestionEventData as AddQuestionEventData
import Common.Api.Models.Event.AddReferenceEventData as AddReferenceEventData
import Common.Api.Models.Event.AddResourceCollectionEventData as AddResourceCollectionEventData
import Common.Api.Models.Event.AddResourcePageEventData as AddResourcePageEventData
import Common.Api.Models.Event.AddTagEventData as AddTagEventData
import Common.Api.Models.Event.EditEventSetters exposing (setRequestBody, setRequestHeaders, setRequestMethod, setRequestUrl, setTestResponse)
import Common.Api.Models.Event.EditIntegrationApiEventData as EditIntegrationApiEventData
import Common.Api.Models.Event.EditIntegrationEventData exposing (EditIntegrationEventData(..))
import Common.Api.Models.Event.EventContent exposing (EventContent(..))
import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.KnowledgeModel.Integration as Integration exposing (Integration)
import Common.Api.Models.KnowledgeModel.Integration.KeyValuePair as KeyValuePair
import Common.Api.Models.KnowledgeModel.Question as Question
import Common.Api.Models.KnowledgeModel.Tag exposing (Tag)
import Common.Api.Models.KnowledgeModelEditor.KnowledgeModelEditorState as KnowledgeModelEditorState
import Common.Api.Models.KnowledgeModelEditorDetail exposing (KnowledgeModelEditorDetail)
import Common.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Common.Api.Models.KnowledgeModelSecret exposing (KnowledgeModelSecret)
import Common.Api.Models.OnlineUserInfo exposing (OnlineUserInfo)
import Common.Api.Models.Prefab exposing (Prefab)
import Common.Api.Models.ProjectCommon as ProjectCommon
import Common.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire
import Common.Api.Models.TypeHint exposing (TypeHint)
import Common.Api.Models.TypeHintRequest as TypeHintRequest
import Common.Api.Models.TypeHintTestResponse exposing (TypeHintTestResponse)
import Common.Api.Models.UrlCheckResponse exposing (UrlCheckResponse)
import Common.Api.Models.WebSockets.ClientKnowledgeModelEditorMessage as ClientKnowledgeModelEditorMessage
import Common.Api.Models.WebSockets.KnowledgeModelEditorMessage.SetContentKnowledgeModelEditorMessage as SetContentKnowledgeModelEditorMessage exposing (SetContentKnowledgeModelEditorMessage)
import Common.Api.Models.WebSockets.ServerKnowledgeModelEditorMessage as ServerKnowledgeModelEditorMessage
import Common.Api.Models.WebSockets.WebSocketServerAction as WebSocketServerAction
import Common.Api.Prefabs as PrefabsApi
import Common.Api.ServerError as ServerError exposing (ServerError)
import Common.Api.TypeHints as TypeHintsApi
import Common.Api.UrlCheck as UrlCheckApi
import Common.Api.WebSocket as WebSocket exposing (WebSocket)
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.DetailPage as DetailPage
import Common.Components.Form as CommonForm
import Common.Components.Form.Field as FormField
import Common.Components.GuideLink as GuideLink
import Common.Components.Markdown as Markdown
import Common.Components.OnlineUsers as OnlineUsers
import Common.Components.Page as Page
import Common.Components.PluginView as PluginView
import Common.Components.Questionnaire as Questionnaire
import Common.Components.SavingIndicator as SavingIndicator
import Common.Components.UiWords as UiWords
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.BuiltInIntegration as BuiltInIntegration
import Common.Data.Forms.KnowledgeModelEditorEditForm as KnowledgeModelEditorEditForm exposing (KnowledgeModelEditorEditForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.NotFound as NotFound
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.KMEditor.Editor.EditorContext as EditorContext exposing (EditorContext)
import Common.Pages.KMEditor.Editor.Entity as Entity
import Common.Pages.KMEditor.Editor.Integration as IntegrationForm
import Common.Pages.KMEditor.Editor.Move as Move
import Common.Pages.KMEditor.Editor.Panels as Panels
import Common.Pages.KMEditor.Editor.Routes exposing (KMEditorRoute(..))
import Common.Pages.KMEditor.Editor.Tabs as Tabs
import Common.Pages.KMEditor.Editor.Tree as Tree
import Common.Ports.Dom as Dom
import Common.Ports.Form as FormPort
import Common.Ports.Window as Window
import Common.Routes as Routes
import Common.Utils.CurlUtils as CurlUtils
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Json.Encode as E
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Set
import String.Format as String
import Task
import Task.Extra as Task
import Ui.Alert as Alert
import Ui.Badge as Badge
import Ui.Breadcrumbs as Breadcrumbs
import Ui.Button as Button
import Ui.Card as Card
import Ui.Copy as Copy
import Ui.DescriptionList as DescriptionList
import Ui.Field as Field
import Ui.Icon as Icon
import Ui.Illustration as Illustration
import Ui.Link as Link
import Ui.Menu as Menu
import Ui.Modal as Modal
import Ui.Page.Message as Message
import Ui.Page.Workspace as Workspace
import Ui.ReorderableList as ReorderableList
import Ui.Resizer as Resizer
import Ui.Text as Text
import Uuid exposing (Uuid)
import Uuid.Extra as Uuid
import Version



-- MODEL


type alias Model =
    { uuid : Uuid
    , mbEntityUuid : Maybe Uuid
    , websocket : WebSocket
    , offline : Bool

    -- Why the server refused a change sent over the socket. The change is
    -- already in the local copy, so the editor and the server no longer agree
    -- until the page is refreshed.
    , saveError : Maybe { message : String, details : List String }
    , onlineUsers : List OnlineUserInfo
    , savingActionUuids : List Uuid
    , saving : SavingIndicator.Model
    , route : KMEditorRoute
    , editorContext : ActionResult EditorContext
    , integrationPrefabs : ActionResult (List Integration)

    -- What each integration the client has built in loaded for its fields,
    -- by its key (`BuiltInIntegration.key`).
    , builtInLoaded : Dict String (ActionResult E.Value)
    , secrets : ActionResult (List KnowledgeModelSecret)
    , navResizer : Resizer.Model
    , asideResizer : Resizer.Model
    , panel : Maybe Panels.Panel
    , urlChecker : Panels.UrlChecker
    , trailMenu : Menu.Model
    , childDrag : Maybe ReorderableList.Dragging
    , navOverlayOpen : Bool
    , copy : Copy.Model
    , moveState : Maybe Move.State
    , deleteState : Maybe { kind : String, uuid : String, event : EventContent }
    , deleteEditorOpen : Bool

    -- The integration editor's working state: a test in flight per
    -- integration, what a template renders to, which template editors are
    -- showing that rather than the template, where a caret was left, and the
    -- cURL command being imported.
    , integrationTests : Dict String (ActionResult TypeHintTestResponse)
    , integrationPreviews : Dict String (ActionResult (List TypeHint))
    , templatePreviews : Set.Set String
    , cursors : Dict String ( Int, Int )

    -- What is being typed into a field whose stored value cannot render it
    -- back, which is every number: see `Entity.Config`.
    , drafts : Dict String String

    -- The field of the warning just followed, which the entity it belongs to
    -- opens on. Cleared as soon as it is used: it says why this navigation
    -- happened, not anything lasting about the entity.
    , followingWarning : Maybe String
    , curlImport : Maybe { uuid : String, curl : String }

    -- The last event sent for each entity, so the next one can be squashed onto
    -- it, and the debounce that holds it back while typing continues.
    , settingsForm : Form FormError KnowledgeModelEditorEditForm
    , savingSettings : ActionResult String
    , deletingEditor : ActionResult String
    , preview : Maybe Questionnaire.Model
    , previewTags : List String
    , publishOpen : Bool
    , publishing : ActionResult String
    , lastEvents : Dict String Event
    , eventDebounces : Dict String (Debounce SetContentKnowledgeModelEditorMessage)
    }


initialModel : AppState -> Uuid -> Maybe Uuid -> Model
initialModel appState uuid mbEntityUuid =
    { uuid = uuid
    , mbEntityUuid = mbEntityUuid
    , websocket = KnowledgeModelEditorsApi.websocket uuid
    , offline = False
    , saveError = Nothing
    , onlineUsers = []
    , savingActionUuids = []
    , saving = SavingIndicator.init
    , route = Edit Nothing
    , editorContext = Loading
    , integrationPrefabs = Loading
    , builtInLoaded = Dict.empty
    , secrets = Loading
    , navResizer = Resizer.init (navResizerConfig appState)
    , asideResizer = Resizer.init (asideResizerConfig appState)
    , panel = Nothing
    , urlChecker = Panels.urlCheckerInit
    , trailMenu = Menu.init
    , childDrag = Nothing
    , navOverlayOpen = False
    , copy = Copy.init
    , moveState = Nothing
    , deleteState = Nothing
    , deleteEditorOpen = False
    , integrationTests = Dict.empty
    , integrationPreviews = Dict.empty
    , templatePreviews = Set.empty
    , cursors = Dict.empty
    , drafts = Dict.empty
    , followingWarning = Nothing
    , curlImport = Nothing
    , settingsForm = KnowledgeModelEditorEditForm.initEmpty appState
    , savingSettings = Unset
    , deletingEditor = Unset
    , preview = Nothing
    , previewTags = []
    , publishOpen = False
    , publishing = Unset
    , lastEvents = Dict.empty
    , eventDebounces = Dict.empty
    }


navResizerConfig : AppState -> Resizer.Config
navResizerConfig appState =
    { edge = Resizer.Trailing
    , label = gettext "Resize the knowledge model" appState.locale
    , min = 240
    , max = 640
    , default = 320
    , collapse = Nothing
    }


asideResizerConfig : AppState -> Resizer.Config
asideResizerConfig appState =
    { edge = Resizer.Leading
    , label = gettext "Resize panel" appState.locale
    , min = 288
    , max = 720
    , default = 360
    , collapse = Nothing
    }


init : AppState -> Uuid -> KMEditorRoute -> Model -> ( Model, Cmd Msg )
init appState uuid route model =
    let
        mbEntityUuid =
            case route of
                Edit mbUuid ->
                    mbUuid

                _ ->
                    Nothing
    in
    if ActionResult.unwrap False (\editorContext -> editorContext.kmEditor.uuid == uuid) model.editorContext then
        -- The same editor, a different tab appState or entity: it is a move inside the
        -- editor, not a reload, so nothing is fetched and nothing torn down.
        --
        -- Opening something from the tree puts the caret in its first field:
        -- the reason to open an entity is to change it, and the alternative is
        -- a trip back across the pane with the mouse. Only when the entity
        -- actually changed, so a tab appState or the back button does not steal focus.
        ( { model
            | route = route

            -- The other tabs keep the entity left open, so coming back to it
            -- through the tab is not taken for opening it anew.
            , mbEntityUuid =
                case route of
                    Edit _ ->
                        mbEntityUuid

                    _ ->
                        model.mbEntityUuid

            -- A dialog is about the page it was opened over. Moving to another
            -- tab appState — by the link inside it, or by the back button — leaves it
            -- with nothing to be about.
            , publishOpen = False

            -- Used by the focus above and then done with: it says why this
            -- navigation happened, not anything lasting about the entity.
            , followingWarning = Nothing

            -- Only the Edit tab appState says which entity is open. The others leave it
            -- where it was, which is how the preview knows the question to open
            -- on and the tree knows where to go back to.
            , editorContext =
                case route of
                    Edit _ ->
                        ActionResult.map (EditorContext.setActiveEditor (Maybe.map Uuid.toString mbEntityUuid)) model.editorContext

                    _ ->
                        model.editorContext
          }
        , Cmd.batch
            [ WebSocket.open appState model.websocket
            , Task.dispatch EnterTab
            , case route of
                Edit _ ->
                    if mbEntityUuid /= model.mbEntityUuid || Maybe.isJust model.followingWarning then
                        focusOnOpen model mbEntityUuid

                    else
                        Cmd.none

                _ ->
                    -- There is no entity pane on the other tabs, so asking for
                    -- its first field leaves the port waiting for a form that
                    -- is not coming — and racing the question the preview puts
                    -- the reader on.
                    Cmd.none
            ]
        )

    else
        let
            initial =
                initialModel appState uuid mbEntityUuid
        in
        ( { initial | route = route }
        , Cmd.batch
            [ KnowledgeModelEditorsApi.getKnowledgeModelEditor appState uuid GotEditor
            , PrefabsApi.getIntegrationPrefabs appState GotIntegrationPrefabs
            , Cmd.batch
                (List.map
                    (\builtIn -> Task.attempt (GotBuiltInLoaded (BuiltInIntegration.key builtIn)) (builtIn.load (BuiltInIntegration.requestContext appState appState.workspaceUuid)))
                    appState.extensions.integrations
                )
            , KnowledgeModelSecretsApi.getKnowledgeModelSecrets appState GotSecrets
            ]
        )


{-| Where the caret goes when an entity is opened.

Its first field, normally: the reason to open an entity is to change it, and the
alternative is a trip back across the pane with the mouse.

Following a warning is the exception. The warning says which field it is about,
and landing on the title of a form whose e-mail is the problem says nothing
about what to fix — so the caret goes to the field that warning names. It has to
be the warning that was followed rather than any warning the entity has: an
integration with no test data and a request URL missing its `{{ q }}` has two,
about two different fields, and the one to answer is the one that was clicked.

The target is scrolled to as well as focused, because it can be far down a long
form, and it is flashed because a focus ring is not drawn for a focus that came
from script after a click — an "Add answer" button would otherwise take the
focus and show nothing for it.

-}
focusOnOpen : Model -> Maybe Uuid -> Cmd Msg
focusOnOpen model mbEntityUuid =
    case model.followingWarning of
        Just field ->
            let
                selector =
                    "#" ++ Entity.fieldId (Maybe.unwrap "" Uuid.toString mbEntityUuid) field
            in
            Dom.scrollToAndFocus { selector = selector, focus = selector, smooth = False }

        Nothing ->
            FormPort.focusFirstFieldIn Entity.paneId


{-| Entering a tab appState does a little work: Preview builds a throwaway questionnaire
from the knowledge model as it stands, so what is previewed is what is in the
editor rather than what was published. The others do nothing.

It is a message rather than part of `init` because building it needs a random
seed and returns a command — the questionnaire is opened on a question, which
means fabricating the answers that lead to it and then scrolling there. It is
sent on every entry into the editor and again when the editor itself arrives,
so landing on the preview's own URL builds it too.

-}
enterTab : AppState -> Model -> ( Seed, Model, Cmd Msg )
enterTab appState model =
    case ( model.route, model.editorContext ) of
        ( Preview, Success editorContext ) ->
            let
                -- What is previewed is the knowledge model as the editor has it
                -- now, minus what has been deleted, and minus the questions the
                -- chosen tags leave out.
                knowledgeModel =
                    EditorContext.getFilteredKM editorContext

                questionnaire =
                    ProjectQuestionnaire.createQuestionnaireDetail
                        KnowledgeModelPackage.dummy
                        knowledgeModel

                -- The values saved for this editor are the ground the preview
                -- starts from; anything typed and not saved is not kept. The
                -- previewed project is in the editor's workspace, as the
                -- project made from it would be.
                withReplies =
                    { questionnaire | replies = editorContext.kmEditor.replies, workspaceUuid = editorContext.kmEditor.workspaceUuid }

                questionUuid =
                    EditorContext.getActiveQuestionUuid editorContext

                -- A question is reached only through the answers above it, so
                -- opening the preview on the question being edited means
                -- answering its way down to it: the answer that reveals it, an
                -- item in each list it sits in. Saved values that already lead
                -- there are left alone, so a preview filled in by hand and then
                -- saved is not undone by looking at a question inside it.
                ( seed, mbPath, generated ) =
                    if String.isEmpty questionUuid then
                        ( appState.seed, Nothing, withReplies )

                    else
                        let
                            ( nextSeed, _, withGenerated ) =
                                ProjectQuestionnaire.generateReplies appState.currentTime appState.seed questionUuid knowledgeModel withReplies
                        in
                        ( nextSeed
                        , ProjectQuestionnaire.getClosestQuestionParentPath withGenerated (KnowledgeModel.createParentMap knowledgeModel) questionUuid
                        , withGenerated
                        )

                ( preview, previewCmd ) =
                    Questionnaire.init appState
                        { generated | knowledgeModel = KnowledgeModel.filterWithTags model.previewTags knowledgeModel }
                        mbPath
                        Nothing

                -- No question to open on — a chapter, a metric, the knowledge
                -- model itself. The chapter it belongs to is still better than
                -- the first one.
                chapterCmd =
                    let
                        chapterUuid =
                            EditorContext.getChapterUuid editorContext.activeUuid editorContext
                    in
                    if Maybe.isNothing mbPath && not (String.isEmpty chapterUuid) then
                        Task.dispatch (PreviewMsg (Questionnaire.openChapterMsg chapterUuid))

                    else
                        Cmd.none
            in
            ( seed
            , { model | preview = Just preview }
            , Cmd.batch [ Cmd.map PreviewMsg previewCmd, chapterCmd ]
            )

        _ ->
            ( appState.seed, model, Cmd.none )


{-| Closing the socket, which the feature router does when the editor is left.
-}
close : Model -> Cmd Msg
close model =
    WebSocket.close model.websocket


{-| Anything still on its way is a reason not to leave — including a keystroke
still waiting for the debounce, which has not been sent at all.

The settings are the one thing in the editor that is not saved as it is typed,
so a form with something in it is a reason of its own — and that one holds
inside the editor as well, because moving to another tab appState throws it away.

-}
isGuarded : AppState -> Routes.Route -> Model -> Maybe String
isGuarded appState nextRoute model =
    if
        (model.route == Settings && Tabs.settingsChanged model.settingsForm)
            && not (Routes.isKmEditorEditorSettings model.uuid nextRoute)
    then
        Just (gettext "There are unsaved changes." appState.locale)

    else if List.isEmpty model.savingActionUuids && Dict.isEmpty model.lastEvents then
        Nothing

    else if Routes.isKmEditorEditor model.uuid nextRoute then
        Nothing

    else
        Just (gettext "Some changes are still saving." appState.locale)


secretNames : Model -> List String
secretNames model =
    ActionResult.unwrap [] (List.map .name) model.secrets



-- UPDATE


type Msg
    = GotEditor (Result ApiError KnowledgeModelEditorDetail)
    | GotIntegrationPrefabs (Result ApiError (List (Prefab Integration)))
    | GotBuiltInLoaded String (Result ApiError E.Value)
    | GotSecrets (Result ApiError (List KnowledgeModelSecret))
    | WebSocketMsg WebSocket.RawMsg
    | WebSocketPing
    | SavingIndicatorMsg SavingIndicator.Msg
    | NavResizerMsg Resizer.Msg
    | AsideResizerMsg Resizer.Msg
    | SetPanel (Maybe Panels.Panel)
    | CheckUrls (List String)
    | GotUrlCheck (Result ApiError UrlCheckResponse)
    | SettingsFormMsg Form.Msg
    | SaveSettings
    | SettingsSetVersion FormField.VersionSuggestion
    | DiscardSettings
    | SavedSettings (Result ApiError ())
    | SetDeleteEditorOpen Bool
    | DeleteEditor
    | DeletedEditor (Result ApiError ())
    | FollowWarning { url : String, field : Maybe String, follow : Bool }
    | SetDraft String String Msg
    | ClearDraft String
    | EnterTab
    | PreviewMsg Questionnaire.Msg
    | PreviewToggleTag String
    | PreviewSelectAllTags
    | PreviewSelectNoneTags
    | SavePreviewReplies
    | SetPublishOpen Bool
    | Publish
    | Published (Result ApiError KnowledgeModelPackage)
    | Refresh
    | CloseSaveError
    | SelectEntity String
    | ToggleNode String Bool
    | ExpandAll
    | CollapseAll
    | TrailMenuMsg Int Menu.Msg
    | SetChildDrag (Maybe ReorderableList.Dragging)
    | SetNavOverlay Bool
    | CopyMsg Copy.Msg
    | OpenMove Move.MovingEntity String
    | MoveSelect String
    | MoveToggle String Bool
    | MoveExpandAll
    | MoveCollapseAll
    | CloseMove
    | ConfirmMove
    | TestIntegration String String (Dict String String)
    | TestedIntegration String (Result ApiError TypeHintTestResponse)
    | PreviewTemplate String String
    | PreviewedTemplate String (Result ApiError (List TypeHint))
    | ShowTemplate String
    | SetCursor String ( Int, Int )
    | AfterInsert { fieldId : String, caret : Int } Msg
    | OpenCurlImport String
    | SetCurlString String
    | CloseCurlImport
    | ConfirmCurlImport
    | OpenDelete { kind : String, uuid : String, event : EventContent }
    | CloseDelete
    | ConfirmDelete
    | AddEntity Tree.AddTo String
    | EventMsg Bool (Maybe String) String (Maybe String) EventContent
    | EventDebounceMsg String Debounce.Msg
    | EventSent Uuid String


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Seed, Model, Cmd msg )
update cfg appState msg model =
    let
        plain ( newModel, cmd ) =
            ( appState.seed, newModel, Cmd.map cfg.wrapMsg cmd )

        quiet newModel =
            ( appState.seed, newModel, Cmd.none )
    in
    case msg of
        GotEditor result ->
            case result of
                Ok kmEditor ->
                    if KnowledgeModelEditorState.isEditable kmEditor.state then
                        ( appState.seed
                        , { model
                            | editorContext =
                                Success
                                    (EditorContext.computeWarnings
                                        { locale = appState.locale, builtIns = appState.extensions.integrations, secrets = secretNames model }
                                        (EditorContext.init kmEditor model.mbEntityUuid)
                                    )
                            , settingsForm = KnowledgeModelEditorEditForm.init appState kmEditor
                          }
                        , Cmd.batch
                            [ Cmd.map cfg.wrapMsg
                                (Cmd.batch
                                    [ WebSocket.open appState model.websocket

                                    -- The tab appState was entered before there was anything
                                    -- to show it from, which is what landing on the
                                    -- preview's own URL does.
                                    , Task.dispatch EnterTab
                                    ]
                                )
                            , UpdateConfig.entityWorkspace cfg kmEditor.workspaceUuid
                            ]
                        )

                    else
                        -- A migration is running: it is the only thing that can
                        -- be done to this editor until it is finished.
                        ( appState.seed, model, Routes.navigate appState (Routes.kmEditorMigration model.uuid) )

                Err error ->
                    if ApiError.isNotFound error then
                        ( appState.seed
                        , model
                        , UpdateConfig.notFound cfg (NotFound.knowledgeModelEditor appState)
                        )

                    else
                        ( appState.seed
                        , { model | editorContext = ApiError.toActionResult appState (gettext "Unable to get the knowledge model editor." appState.locale) error }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )

        GotIntegrationPrefabs result ->
            case result of
                Ok prefabs ->
                    quiet { model | integrationPrefabs = Success (List.map .content prefabs) }

                Err _ ->
                    -- The prefabs are a convenience in the integration form;
                    -- failing to load them is not an error the editor shows.
                    quiet { model | integrationPrefabs = Error "" }

        GotBuiltInLoaded builtInKey result ->
            quiet
                { model
                    | builtInLoaded =
                        Dict.insert builtInKey
                            (case result of
                                Ok value ->
                                    Success value

                                Err _ ->
                                    -- The fields say it in their own words.
                                    Error ""
                            )
                            model.builtInLoaded
                }

        GotSecrets result ->
            case result of
                Ok secrets ->
                    quiet
                        { model
                            | secrets = Success secrets
                            , editorContext =
                                ActionResult.map
                                    (EditorContext.computeWarnings { locale = appState.locale, builtIns = appState.extensions.integrations, secrets = List.map .name secrets })
                                    model.editorContext
                        }

                Err error ->
                    quiet { model | secrets = ApiError.toActionResult appState (gettext "Unable to get knowledge model secrets." appState.locale) error }

        WebSocketMsg websocketMsg ->
            plain (handleWebSocketMsg appState websocketMsg model)

        WebSocketPing ->
            plain ( model, WebSocket.ping model.websocket )

        SavingIndicatorMsg savingMsg ->
            quiet { model | saving = SavingIndicator.update savingMsg model.saving }

        NavResizerMsg resizerMsg ->
            quiet { model | navResizer = Resizer.update resizerMsg model.navResizer }

        AsideResizerMsg resizerMsg ->
            quiet { model | asideResizer = Resizer.update resizerMsg model.asideResizer }

        SetPanel panel ->
            quiet { model | panel = panel }

        CheckUrls urls ->
            plain ( { model | urlChecker = Loading }, UrlCheckApi.postUrlCheck appState { urls = urls } GotUrlCheck )

        GotUrlCheck result ->
            quiet { model | urlChecker = Panels.check appState result }

        SettingsFormMsg formMsg ->
            quiet { model | settingsForm = Form.update (KnowledgeModelEditorEditForm.validation appState) formMsg model.settingsForm }

        SettingsSetVersion suggestion ->
            quiet
                { model
                    | settingsForm =
                        FormField.setVersion (KnowledgeModelEditorEditForm.validation appState)
                            { majorField = "versionMajor", minorField = "versionMinor", patchField = "versionPatch" }
                            suggestion
                            model.settingsForm
                }

        DiscardSettings ->
            quiet { model | settingsForm = settingsFormOf appState model, savingSettings = Unset }

        SaveSettings ->
            case Form.getOutput model.settingsForm of
                Just form ->
                    plain
                        ( { model | savingSettings = Loading }
                        , KnowledgeModelEditorsApi.putKnowledgeModelEditor appState model.uuid (KnowledgeModelEditorEditForm.encode form) SavedSettings
                        )

                Nothing ->
                    quiet { model | settingsForm = Form.update (KnowledgeModelEditorEditForm.validation appState) Form.Submit model.settingsForm }

        SavedSettings result ->
            case result of
                Ok _ ->
                    -- The wizard reloads the whole page here, which drops the
                    -- socket and everybody's place. The saved values are applied
                    -- to the editor in memory instead.
                    let
                        saved =
                            { model
                                | savingSettings = Success ""
                                , editorContext = ActionResult.map (applySettings model.settingsForm) model.editorContext
                            }
                    in
                    -- Built again from what was saved, so the form holds no
                    -- changed fields and the bar that offers to save them goes.
                    quiet { saved | settingsForm = settingsFormOf appState saved }

                Err error ->
                    ( appState.seed
                    , { model | savingSettings = ApiError.toActionResult appState (gettext "Unable to save the settings." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        SetDeleteEditorOpen open ->
            quiet { model | deleteEditorOpen = open, deletingEditor = Unset }

        DeleteEditor ->
            plain
                ( { model | deletingEditor = Loading }
                , KnowledgeModelEditorsApi.deleteKnowledgeModelEditor appState model.uuid DeletedEditor
                )

        DeletedEditor result ->
            case result of
                Ok _ ->
                    ( appState.seed, model, Routes.navigate appState Routes.kmEditorIndex )

                Err error ->
                    ( appState.seed
                    , { model | deletingEditor = ApiError.toActionResult appState (gettext "Unable to delete the knowledge model editor." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        SetPublishOpen open ->
            quiet { model | publishOpen = open, publishing = Unset }

        Publish ->
            plain
                ( { model | publishing = Loading }
                , KnowledgeModelPackagesApi.postFromKnowledgeModelEditor appState model.uuid Nothing Published
                )

        Published result ->
            case result of
                Ok kmPackage ->
                    ( appState.seed, model, Routes.navigate appState (Routes.knowledgeModelsDetail kmPackage.uuid) )

                Err error ->
                    ( appState.seed
                    , { model | publishing = ApiError.toActionResult appState (gettext "Unable to publish the knowledge model." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        FollowWarning { url, field, follow } ->
            -- The field is set before the page changes, because the entity it
            -- opens reads it as it opens. A click with a modifier is the browser
            -- opening a new tab appState, and nothing is followed in this one.
            if follow then
                ( appState.seed
                , { model | followingWarning = field }
                , Nav.pushUrl appState.key url
                )

            else
                quiet { model | followingWarning = Nothing }

        SetDraft key text inner ->
            update cfg appState inner { model | drafts = Dict.insert key text model.drafts }

        ClearDraft key ->
            quiet { model | drafts = Dict.remove key model.drafts }

        EnterTab ->
            let
                ( seed, entered, cmd ) =
                    enterTab appState model
            in
            ( seed, entered, Cmd.map cfg.wrapMsg cmd )

        PreviewToggleTag tagUuid ->
            let
                tags =
                    if List.member tagUuid model.previewTags then
                        List.filter ((/=) tagUuid) model.previewTags

                    else
                        model.previewTags ++ [ tagUuid ]
            in
            quiet (setPreviewTags tags model)

        PreviewSelectAllTags ->
            case model.editorContext of
                Success editorContext ->
                    quiet
                        (setPreviewTags
                            (EditorContext.filterDeleted editorContext editorContext.kmEditor.knowledgeModel.tagUuids)
                            model
                        )

                _ ->
                    quiet model

        PreviewSelectNoneTags ->
            quiet (setPreviewTags [] model)

        SavePreviewReplies ->
            case model.preview of
                Just preview ->
                    let
                        ( actionUuid, seed ) =
                            Uuid.step appState.seed

                        message =
                            ClientKnowledgeModelEditorMessage.SetReplies
                                { uuid = actionUuid
                                , replies = preview.questionnaire.replies
                                }
                    in
                    ( seed
                    , addSavingActionUuid actionUuid model
                    , Cmd.map cfg.wrapMsg
                        (Cmd.batch
                            [ WebSocket.send model.websocket (ClientKnowledgeModelEditorMessage.encode message)
                            , Window.setUnloadMessage (gettext "Some changes are still saving." appState.locale)
                            ]
                        )
                    )

                Nothing ->
                    quiet model

        PreviewMsg previewMsg ->
            case model.preview of
                Just preview ->
                    let
                        return =
                            Questionnaire.update appState
                                { wrapMsg = cfg.wrapMsg << PreviewMsg
                                , mbKmEditorUuid = Just model.uuid
                                , projectCommon = ProjectCommon.dummy
                                , previewVersionMsg = Nothing
                                , revertVersionMsg = Nothing
                                }
                                previewMsg
                                preview
                    in
                    ( return.seed, { model | preview = Just return.model }, return.cmd )

                Nothing ->
                    quiet model

        Refresh ->
            plain ( model, Window.refresh () )

        CloseSaveError ->
            quiet { model | saveError = Nothing }

        SelectEntity uuid ->
            -- The tree navigates rather than setting state, so the back button
            -- walks the editing history. On a narrow page the tree is an
            -- overlay over the form it is about to open, so it gets out of the
            -- way once something is chosen.
            ( appState.seed
            , { model | navOverlayOpen = False }
            , Routes.navigate appState (Routes.kmEditorEditor model.uuid (entityRoute uuid model))
            )

        ToggleNode uuid open ->
            quiet { model | editorContext = ActionResult.map (EditorContext.setNodeOpen uuid open) model.editorContext }

        ExpandAll ->
            quiet { model | editorContext = ActionResult.map EditorContext.treeExpandAll model.editorContext }

        CollapseAll ->
            quiet { model | editorContext = ActionResult.map EditorContext.treeCollapseAll model.editorContext }

        SetChildDrag drag ->
            quiet { model | childDrag = drag }

        SetNavOverlay isOpen ->
            -- On a narrow page the tree is an overlay, and focus follows it:
            -- into it when it opens, back to the control that opened it when
            -- it closes.
            plain
                ( { model | navOverlayOpen = isOpen }
                , Dom.focus
                    ("#"
                        ++ (if isOpen then
                                Workspace.navId workspaceId

                            else
                                Workspace.navToggleId workspaceId
                           )
                    )
                )

        CopyMsg copyMsg ->
            let
                ( copy, copyCmd ) =
                    Copy.update copyMsg model.copy
            in
            plain ( { model | copy = copy }, Cmd.map CopyMsg copyCmd )

        OpenMove entity uuid ->
            quiet { model | moveState = ActionResult.unwrap Nothing (Just << Move.open entity uuid) model.editorContext }

        MoveSelect uuid ->
            quiet { model | moveState = Maybe.map (Move.setSelected uuid) model.moveState }

        MoveToggle uuid isOpen ->
            quiet { model | moveState = Maybe.map (moveSetNodeOpen uuid isOpen) model.moveState }

        MoveExpandAll ->
            quiet { model | moveState = Maybe.map2 Move.expandAll (ActionResult.toMaybe model.editorContext) model.moveState }

        MoveCollapseAll ->
            quiet { model | moveState = Maybe.map Move.collapseAll model.moveState }

        CloseMove ->
            quiet { model | moveState = Nothing }

        ConfirmMove ->
            -- A move is structural, so it is never debounced: squashed onto the
            -- edit before it, it would be a different knowledge model.
            case Maybe.andThen (\state -> Maybe.map (Tuple.pair state) (Move.event state)) model.moveState of
                Just ( state, content ) ->
                    update cfg
                        appState
                        (EventMsg False Nothing state.parentUuid (Just state.uuid) content)
                        { model | moveState = Nothing }

                Nothing ->
                    quiet model

        TestIntegration integrationUuid q variables ->
            plain
                ( { model | integrationTests = Dict.insert integrationUuid Loading model.integrationTests }
                , TypeHintsApi.testTypeHints appState model.uuid integrationUuid q variables (TestedIntegration integrationUuid)
                )

        TestedIntegration integrationUuid result ->
            case result of
                Ok response ->
                    -- The response is kept on the integration rather than in
                    -- this model: it is what the Response section is built
                    -- from, and it has to survive a reload like any other part
                    -- of the knowledge model.
                    update cfg
                        appState
                        (EventMsg False
                            Nothing
                            (parentOf integrationUuid model)
                            (Just integrationUuid)
                            (EditIntegrationEvent (EditIntegrationApiEvent (EditIntegrationApiEventData.init |> setTestResponse (Just response))))
                        )
                        { model | integrationTests = Dict.insert integrationUuid (Success response) model.integrationTests }

                Err error ->
                    quiet
                        { model
                            | integrationTests =
                                Dict.insert integrationUuid
                                    (ApiError.toActionResult appState (gettext "Unable to get the test result." appState.locale) error)
                                    model.integrationTests
                        }

        PreviewTemplate integrationUuid name ->
            let
                key =
                    integrationUuid ++ ":" ++ name
            in
            plain
                ( { model
                    | templatePreviews = Set.insert key model.templatePreviews
                    , integrationPreviews = Dict.insert key Loading model.integrationPreviews
                  }
                , TypeHintsApi.fetchTypeHints appState
                    (TypeHintRequest.fromKmEditorIntegration model.uuid (Uuid.fromUuidString integrationUuid))
                    (PreviewedTemplate key)
                )

        PreviewedTemplate key result ->
            quiet
                { model
                    | integrationPreviews =
                        Dict.insert key
                            (case result of
                                Ok hints ->
                                    Success hints

                                Err error ->
                                    ApiError.toActionResult appState (gettext "Unable to get the preview." appState.locale) error
                            )
                            model.integrationPreviews
                }

        ShowTemplate key ->
            quiet { model | templatePreviews = Set.remove key model.templatePreviews }

        SetCursor name selection ->
            quiet { model | cursors = Dict.insert name selection model.cursors }

        AfterInsert where_ inner ->
            -- The edit, and then the caret put back where the inserted text
            -- ends: a value dropped into a template is the middle of writing
            -- one, not the end of it.
            let
                ( seed, newModel, cmd ) =
                    update cfg appState inner model
            in
            ( seed
            , { newModel | cursors = Dict.insert where_.fieldId ( where_.caret, where_.caret ) newModel.cursors }
            , Cmd.batch
                [ cmd
                , Dom.focusWithCaret { selector = "#" ++ where_.fieldId, caret = where_.caret }
                ]
            )

        OpenCurlImport integrationUuid ->
            quiet { model | curlImport = Just { uuid = integrationUuid, curl = "" } }

        SetCurlString curl ->
            quiet { model | curlImport = Maybe.map (\state -> { state | curl = curl }) model.curlImport }

        CloseCurlImport ->
            quiet { model | curlImport = Nothing }

        ConfirmCurlImport ->
            case model.curlImport of
                Just state ->
                    let
                        parsed =
                            CurlUtils.parseCurl state.curl

                        withBody =
                            if String.isEmpty parsed.body then
                                identity

                            else
                                setRequestBody (Just parsed.body)
                    in
                    update cfg
                        appState
                        (EventMsg False
                            Nothing
                            (parentOf state.uuid model)
                            (Just state.uuid)
                            (EditIntegrationEvent
                                (EditIntegrationApiEvent
                                    (EditIntegrationApiEventData.init
                                        |> setRequestMethod parsed.method
                                        |> setRequestUrl parsed.url
                                        |> setRequestHeaders (List.map KeyValuePair.fromTuple parsed.headers)
                                        |> withBody
                                    )
                                )
                            )
                        )
                        { model | curlImport = Nothing }

                Nothing ->
                    quiet model

        OpenDelete state ->
            quiet { model | deleteState = Just state }

        CloseDelete ->
            quiet { model | deleteState = Nothing }

        ConfirmDelete ->
            case model.deleteState of
                Just state ->
                    update cfg
                        appState
                        (EventMsg False Nothing (parentOf state.uuid model) (Just state.uuid) state.event)
                        { model | deleteState = Nothing }

                Nothing ->
                    quiet model

        TrailMenuMsg count menuMsg ->
            let
                ( trailMenu, menuCmd ) =
                    Menu.update trailMenuId count menuMsg model.trailMenu
            in
            ( appState.seed, { model | trailMenu = trailMenu }, Cmd.map (cfg.wrapMsg << TrailMenuMsg count) menuCmd )

        AddEntity addTo parentUuid ->
            update cfg appState (EventMsg False Nothing parentUuid Nothing (addEvent addTo)) model

        EventMsg shouldDebounce mbFocusSelector parentUuid mbEntityUuid content ->
            applyAndSend cfg appState shouldDebounce mbFocusSelector parentUuid mbEntityUuid content model

        EventDebounceMsg entityUuid debounceMsg ->
            let
                send message =
                    Cmd.batch
                        [ WebSocket.send model.websocket
                            (ClientKnowledgeModelEditorMessage.encode (ClientKnowledgeModelEditorMessage.SetContent message))
                        , Task.dispatch (EventSent (SetContentKnowledgeModelEditorMessage.getUuid message) entityUuid)
                        ]

                ( debounce, cmd ) =
                    Debounce.update (debounceConfig appState entityUuid)
                        (Debounce.takeLast send)
                        debounceMsg
                        (debounceFor entityUuid model)
            in
            plain ( { model | eventDebounces = Dict.insert entityUuid debounce model.eventDebounces }, cmd )

        EventSent actionUuid entityUuid ->
            -- The debounce has let it go: the squash target is spent, and the
            -- action is now something to wait for.
            quiet (addSavingActionUuid actionUuid { model | lastEvents = Dict.remove entityUuid model.lastEvents })


entityRoute : String -> Model -> Maybe Uuid
entityRoute uuid model =
    ActionResult.unwrap Nothing (EditorContext.getEditUuid uuid) model.editorContext



-- SENDING AN EVENT


applyAndSend :
    UpdateConfig Msg msg
    -> AppState
    -> Bool
    -> Maybe String
    -> String
    -> Maybe String
    -> EventContent
    -> Model
    -> ( Seed, Model, Cmd msg )
applyAndSend cfg appState shouldDebounce mbFocusSelector parentUuid mbEntityUuid content model =
    let
        ( eventUuid, seed1 ) =
            Uuid.stepString appState.seed

        ( actionUuid, seed2 ) =
            Uuid.step seed1

        ( entityUuid, seed3 ) =
            case mbEntityUuid of
                Just uuid ->
                    ( uuid, seed2 )

                Nothing ->
                    Uuid.stepString seed2

        event =
            { uuid = eventUuid
            , parentUuid = parentUuid
            , entityUuid = entityUuid
            , content = content
            , createdAt = appState.currentTime
            }

        editorContext =
            ActionResult.map
                (EditorContext.computeWarnings { locale = appState.locale, builtIns = appState.extensions.integrations, secrets = secretNames model }
                    << EditorContext.applyEvent True event
                )
                model.editorContext

        focusCmd =
            case mbFocusSelector of
                Just selector ->
                    Dom.focus selector

                Nothing ->
                    Cmd.none

        unloadCmd =
            Window.setUnloadMessage (gettext "Some changes are still saving." appState.locale)
    in
    if shouldDebounce then
        let
            squashed =
                case Dict.get entityUuid model.lastEvents of
                    Just lastEvent ->
                        Event.squash lastEvent event

                    Nothing ->
                        event

            message =
                SetContentKnowledgeModelEditorMessage.AddKnowledgeModelEditorWebSocketEvent
                    { uuid = actionUuid, event = squashed }

            ( debounce, debounceCmd ) =
                Debounce.push (debounceConfig appState entityUuid) message (debounceFor entityUuid model)
        in
        ( seed3
        , { model
            | editorContext = editorContext
            , saving = SavingIndicator.saving
            , eventDebounces = Dict.insert entityUuid debounce model.eventDebounces
            , lastEvents = Dict.insert entityUuid squashed model.lastEvents
          }
        , Cmd.batch [ Cmd.map cfg.wrapMsg debounceCmd, unloadCmd, Cmd.map cfg.wrapMsg focusCmd ]
        )

    else
        let
            message =
                SetContentKnowledgeModelEditorMessage.AddKnowledgeModelEditorWebSocketEvent
                    { uuid = actionUuid, event = event }

            sendCmd =
                WebSocket.send model.websocket
                    (ClientKnowledgeModelEditorMessage.encode (ClientKnowledgeModelEditorMessage.SetContent message))

            navigateCmd =
                navigateToActive appState model.uuid model.editorContext editorContext
        in
        ( seed3
        , addSavingActionUuid actionUuid { model | editorContext = editorContext }
        , Cmd.batch [ Cmd.map cfg.wrapMsg sendCmd, unloadCmd, navigateCmd, Cmd.map cfg.wrapMsg focusCmd ]
        )


{-| The settings the form just saved, applied to the editor in memory. They are
not events and nothing else derives from them, so there is nothing to replay.
-}
applySettings : Form FormError KnowledgeModelEditorEditForm -> EditorContext -> EditorContext
applySettings form editorContext =
    case Form.getOutput form of
        Just settings ->
            let
                kmEditor =
                    editorContext.kmEditor
            in
            { editorContext
                | kmEditor =
                    { kmEditor
                        | name = settings.name
                        , description = settings.description
                        , id = settings.id
                        , language = settings.language
                        , license = settings.license
                        , readme = settings.readme
                        , version = Version.create settings.versionMajor settings.versionMinor settings.versionPatch
                    }
            }

        Nothing ->
            editorContext


{-| The settings form as the editor stands, which is how it starts and what
discarding goes back to.
-}
settingsFormOf : AppState -> Model -> Form FormError KnowledgeModelEditorEditForm
settingsFormOf appState model =
    case model.editorContext of
        Success editorContext ->
            KnowledgeModelEditorEditForm.init appState editorContext.kmEditor

        _ ->
            KnowledgeModelEditorEditForm.initEmpty appState


{-| An event that moved the active entity — adding one, or deleting the one you
were in — moves the URL with it.
-}
navigateToActive : AppState -> Uuid -> ActionResult EditorContext -> ActionResult EditorContext -> Cmd msg
navigateToActive appState uuid before after =
    let
        was =
            ActionResult.unwrap "" .activeUuid before

        now =
            ActionResult.unwrap "" .activeUuid after
    in
    if not (String.isEmpty now) && was /= now then
        Routes.navigate appState
            (Routes.kmEditorEditor uuid (ActionResult.unwrap Nothing (EditorContext.getEditUuid now) after))

    else
        Cmd.none


{-| The tags the preview is narrowed by. Only the knowledge model under the
questionnaire changes: the chapter open, what is collapsed and every answer
given stay as they are, so ticking a tag does not throw away the reader's place.
-}
setPreviewTags : List String -> Model -> Model
setPreviewTags tags model =
    case ( model.preview, model.editorContext ) of
        ( Just preview, Success editorContext ) ->
            { model
                | previewTags = tags
                , preview =
                    Just
                        (Questionnaire.setKnowledgeModel
                            (KnowledgeModel.filterWithTags tags (EditorContext.getFilteredKM editorContext))
                            preview
                        )
            }

        _ ->
            { model | previewTags = tags }


addSavingActionUuid : Uuid -> Model -> Model
addSavingActionUuid actionUuid model =
    { model
        | savingActionUuids = actionUuid :: model.savingActionUuids
        , saving = SavingIndicator.saving
    }


debounceConfig : AppState -> String -> Debounce.Config Msg
debounceConfig appState entityUuid =
    { strategy = Debounce.soon appState.websocketThrottleDelay
    , transform = EventDebounceMsg entityUuid
    }


debounceFor : String -> Model -> Debounce SetContentKnowledgeModelEditorMessage
debounceFor entityUuid model =
    Maybe.withDefault Debounce.init (Dict.get entityUuid model.eventDebounces)


addEvent : Tree.AddTo -> EventContent
addEvent addTo =
    case addTo of
        Tree.AddChapter ->
            AddChapterEvent AddChapterEventData.init

        Tree.AddQuestion ->
            AddQuestionEvent AddQuestionEventData.init

        Tree.AddAnswer ->
            AddAnswerEvent AddAnswerEventData.init

        Tree.AddChoice ->
            AddChoiceEvent AddChoiceEventData.init

        Tree.AddReference ->
            AddReferenceEvent AddReferenceEventData.init

        Tree.AddExpert ->
            AddExpertEvent AddExpertEventData.init

        Tree.AddMetric ->
            AddMetricEvent AddMetricEventData.init

        Tree.AddPhase ->
            AddPhaseEvent AddPhaseEventData.init

        Tree.AddTag ->
            AddTagEvent AddTagEventData.init

        Tree.AddIntegration ->
            AddIntegrationEvent AddIntegrationEventData.init

        Tree.AddResourceCollection ->
            AddResourceCollectionEvent AddResourceCollectionEventData.init

        Tree.AddResourcePage ->
            AddResourcePageEvent AddResourcePageEventData.init



-- RECEIVING


handleWebSocketMsg : AppState -> WebSocket.RawMsg -> Model -> ( Model, Cmd Msg )
handleWebSocketMsg appState websocketMsg model =
    case WebSocket.receive (WebSocketServerAction.decoder ServerKnowledgeModelEditorMessage.decoder) websocketMsg model.websocket of
        WebSocket.Message (WebSocketServerAction.Success message) ->
            case message of
                ServerKnowledgeModelEditorMessage.SetUserList users ->
                    ( { model | onlineUsers = users }, Cmd.none )

                ServerKnowledgeModelEditorMessage.SetContent (SetContentKnowledgeModelEditorMessage.AddKnowledgeModelEditorWebSocketEvent data) ->
                    receiveEvent appState data model

                ServerKnowledgeModelEditorMessage.SetReplies data ->
                    let
                        remaining =
                            List.filter ((/=) data.uuid) model.savingActionUuids

                        settled =
                            List.member data.uuid model.savingActionUuids && List.isEmpty remaining
                    in
                    ( { model
                        | savingActionUuids = remaining
                        , editorContext = ActionResult.map (EditorContext.setReplies data.replies) model.editorContext

                        -- Also into the preview itself, so a save made by
                        -- somebody else shows in an open one rather than
                        -- waiting for the next entry into the tab appState.
                        , preview = Maybe.map (Questionnaire.setReplies data.replies) model.preview
                        , saving =
                            if settled then
                                SavingIndicator.saved

                            else
                                model.saving
                      }
                    , if settled then
                        Window.clearUnloadMessage ()

                      else
                        Cmd.none
                    )

        WebSocket.Message (WebSocketServerAction.Error serverError) ->
            -- The error does not say which change it is about, so the changes
            -- still waiting stay waiting; the indicator just stops saying
            -- "Saving..." for one that is not coming.
            ( { model
                | saveError = Just (saveErrorOf appState model serverError)
                , saving = SavingIndicator.init
              }
            , Cmd.none
            )

        WebSocket.Reconnect ->
            ( model, WebSocket.open appState model.websocket )

        WebSocket.Close ->
            ( { model | offline = True }, Cmd.none )

        _ ->
            ( model, Cmd.none )


{-| Why a change was refused: the server's words when it has them, a sentence of
ours when it does not, and when the server says what in the change it could not
accept, a line for each of those.

A rejected event says what was wrong with it field by field, naming entities by
their uuids. They are named here as the tree names them: the change is already
in the local copy, so even an entity the server has never heard of has a name
in it.

-}
saveErrorOf : AppState -> Model -> ServerError -> { message : String, details : List String }
saveErrorOf appState model serverError =
    let
        simple message =
            { message = message, details = [] }
    in
    case serverError of
        ServerError.UserSimpleError message ->
            simple (Maybe.withDefault (gettext "The change could not be saved." appState.locale) (ServerError.messageToReadable appState message))

        ServerError.UserFormError data ->
            { message = gettext "The server rejected the change." appState.locale
            , details =
                data.fieldErrors
                    |> Dict.values
                    |> List.concat
                    |> List.map (eventErrorToReadable appState model)
            }

        ServerError.SystemLogError data ->
            simple (String.format data.defaultMessage data.params)

        ServerError.ForbiddenError ->
            simple (ServerError.forbiddenMessage appState)

        ServerError.TooManyRequestsError data ->
            simple (ServerError.tooManyRequestsMessage appState data)


eventErrorToReadable : AppState -> Model -> ServerError.Message -> String
eventErrorToReadable appState model message =
    let
        name uuid =
            let
                editorName =
                    ActionResult.unwrap "" (EditorContext.getEditorName appState.locale uuid) model.editorContext
            in
            if String.isEmpty editorName then
                "'" ++ uuid ++ "'"

            else
                "\"" ++ editorName ++ "\""
    in
    case ( message.code, message.params ) of
        ( "error.validation.km_event_entity_uniqueness", [ uuid ] ) ->
            String.format (gettext "%s already exists." appState.locale) [ name uuid ]

        ( "error.validation.km_event_entity_absence", [ uuid, kind ] ) ->
            String.format (gettext "%s is not an existing %s." appState.locale) [ name uuid, entityKindToReadable appState kind ]

        ( "error.validation.km_event_parent_mismatch", [ uuid, parentUuid ] ) ->
            String.format (gettext "%s is no longer in the same place: it is in %s now." appState.locale) [ name uuid, name parentUuid ]

        ( "error.validation.km_event_move_cycle", [ uuid, targetUuid ] ) ->
            String.format (gettext "%s cannot be moved under %s, which is the entity itself or inside it." appState.locale) [ name uuid, name targetUuid ]

        ( "error.validation.km_event_children_permutation", _ ) ->
            gettext "The new order does not match the current items: some were added or removed in the meantime." appState.locale

        _ ->
            Maybe.withDefault message.code (ServerError.messageToReadable appState message)


{-| The kind of entity the server expected, in the words the editor uses for
it. One it does not know is shown as it came.
-}
entityKindToReadable : AppState -> String -> String
entityKindToReadable appState kind =
    case String.toLower (String.filter Char.isAlpha kind) of
        "knowledgemodel" ->
            gettext "knowledge model" appState.locale

        "chapter" ->
            gettext "chapter" appState.locale

        "question" ->
            gettext "question" appState.locale

        "answer" ->
            gettext "answer" appState.locale

        "choice" ->
            gettext "choice" appState.locale

        "reference" ->
            gettext "reference" appState.locale

        "expert" ->
            gettext "expert" appState.locale

        "metric" ->
            gettext "metric" appState.locale

        "phase" ->
            gettext "phase" appState.locale

        "tag" ->
            gettext "question tag" appState.locale

        "integration" ->
            gettext "integration" appState.locale

        "resourcecollection" ->
            gettext "resource collection" appState.locale

        "resourcepage" ->
            gettext "resource page" appState.locale

        _ ->
            kind


{-| The server broadcasts every event to everybody, its author included. An
action uuid this client is waiting for is that echo: the change is already
applied, so only the waiting ends. Anything else is somebody else's, and is
applied without moving the cursor.
-}
receiveEvent : AppState -> { uuid : Uuid, event : Event } -> Model -> ( Model, Cmd Msg )
receiveEvent appState data model =
    let
        mine =
            List.member data.uuid model.savingActionUuids

        remaining =
            List.filter ((/=) data.uuid) model.savingActionUuids

        editorContext =
            if mine then
                model.editorContext

            else
                ActionResult.map
                    (EditorContext.computeWarnings { locale = appState.locale, builtIns = appState.extensions.integrations, secrets = secretNames model }
                        << EditorContext.applyEvent False data.event
                    )
                    model.editorContext

        settled =
            mine && List.isEmpty remaining
    in
    ( { model
        | savingActionUuids = remaining
        , editorContext = editorContext
        , saving =
            if settled then
                SavingIndicator.saved

            else
                model.saving
      }
    , if settled then
        Window.clearUnloadMessage ()

      else
        Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    Sub.batch
        [ WebSocket.listen WebSocketMsg
        , WebSocket.schedulePing WebSocketPing
        , Sub.map SavingIndicatorMsg (SavingIndicator.subscriptions model.saving)
        , Sub.map NavResizerMsg (Resizer.subscriptions model.navResizer)
        , Sub.map AsideResizerMsg (Resizer.subscriptions model.asideResizer)
        , Sub.map (TrailMenuMsg 0) (Menu.subscriptions trailMenuId model.trailMenu)

        -- The preview is a whole questionnaire, and everything in it that
        -- answers to something outside itself — the resizer between the
        -- chapters and the questions, the scroll it remembers — needs its
        -- subscriptions to be here. Without them the panel edge is a handle
        -- that does nothing when dragged.
        , case model.preview of
            Just preview ->
                Sub.map PreviewMsg (Questionnaire.subscriptions preview)

            Nothing ->
                Sub.none

        -- The settings are an ordinary form, so Cmd/Ctrl+Enter saves them.
        , if model.route == Settings then
            CommonForm.submitShortcut appState { result = model.savingSettings, onSubmit = SaveSettings }

          else
            Sub.none

        -- Cmd/Ctrl+Enter sends the move once there is somewhere to send it,
        -- which is exactly when the button that does it can be pressed. The
        -- dialog holds a tree rather than a form, so there is no submit for
        -- Enter to find on its own.
        , case model.moveState of
            Just state ->
                if Move.canMove state then
                    CommonForm.submitShortcut appState { result = ActionResult.Unset, onSubmit = ConfirmMove }

                else
                    Sub.none

            Nothing ->
                Sub.none
        ]



-- VIEW


view : AppState -> KMEditorRoute -> Model -> Html Msg
view appState route model =
    if model.offline then
        viewMessage appState (gettext "Disconnected" appState.locale) (gettext "You have been disconnected, try to refresh the page." appState.locale)

    else
        Page.actionResult appState (viewEditor appState route model) model.editorContext


viewMessage : AppState -> String -> String -> Html Msg
viewMessage appState heading line =
    Message.toHtml
        (Message.message { title = heading, description = line }
            |> Message.withIllustration (Illustration.toHtml Illustration.error)
            |> Message.withAction (Button.primary (gettext "Refresh" appState.locale) Refresh)
        )


viewEditor : AppState -> KMEditorRoute -> Model -> EditorContext -> Html Msg
viewEditor appState route model editorContext =
    DetailPage.toHtml
        (DetailPage.detailPage
            { title = editorContext.kmEditor.name
            , content =
                [ viewTab appState route model editorContext
                , viewPublishModal appState model editorContext
                , viewMoveModal appState model editorContext
                , viewDeleteModal appState model editorContext
                , viewDeleteEditorModal appState model editorContext
                , viewCurlImportModal appState model
                , viewSaveErrorModal appState model
                ]
            }
            -- Nobody else here means no group and no rule beside the actions.
            |> applyWhenJust (OnlineUsers.avatars appState model.onlineUsers) DetailPage.withActionsLeading
            |> DetailPage.withHeaderAside (SavingIndicator.toHtml appState model.saving)
            |> DetailPage.withActions
                [ Button.primary (gettext "Publish" appState.locale) (SetPublishOpen True)
                    |> Button.withIcon (Icon.solid "fa-cloud-arrow-up")
                    |> Button.withTestId "km-editor_publish"
                ]
            |> DetailPage.withTabsWithTestIds
                { label = gettext "Knowledge model editor" appState.locale
                , items =
                    List.map (tab appState model.uuid route)
                        -- Back to the entity left open, not the root: going to another
                        -- tab and back is a look aside, not a fresh start.
                        [ ( Edit (EditorContext.getEditUuid editorContext.activeUuid editorContext), gettext "Knowledge Model" appState.locale, ( Icon.solid "fa-sitemap", "km-editor_nav_edit" ) )
                        , ( Phases, gettext "Phases" appState.locale, ( Icon.regular "fa-clock", "km-editor_nav_phases" ) )
                        , ( QuestionTags, gettext "Question Tags" appState.locale, ( Icon.solid "fa-tag", "km-editor_nav_question-tags" ) )
                        , ( Preview, gettext "Preview" appState.locale, ( Icon.solid "fa-eye", "km-editor_nav_preview" ) )
                        , ( Settings, gettext "Settings" appState.locale, ( Icon.solid "fa-gear", "km-editor_nav_settings" ) )
                        ]
                }
            |> DetailPage.withFlushContent True
            -- The settings are the one tab appState with something to save, so the bar
            -- that saves them is there and nowhere else.
            |> (if route == Settings then
                    DetailPage.withFooter
                        (Tabs.settingsFooter appState
                            { form = model.settingsForm
                            , saving = model.savingSettings
                            , onSubmit = SaveSettings
                            , onDiscard = DiscardSettings
                            }
                        )

                else
                    identity
               )
        )


applyWhenJust : Maybe a -> (a -> b -> b) -> b -> b
applyWhenJust mbValue apply value =
    case mbValue of
        Just present ->
            apply present value

        Nothing ->
            value


tab : AppState -> Uuid -> KMEditorRoute -> ( KMEditorRoute, String, ( Icon.Icon, String ) ) -> DetailPage.TabItem
tab appState uuid current ( route, label, ( icon, testId ) ) =
    { label = label
    , url = Routes.toUrl appState (routeOf uuid route)
    , active = sameTab current route
    , badge = Nothing
    , testId = Just testId
    , icon = Just icon
    }


routeOf : Uuid -> KMEditorRoute -> Routes.Route
routeOf uuid route =
    case route of
        Edit mbEntityUuid ->
            Routes.kmEditorEditor uuid mbEntityUuid

        Phases ->
            Routes.kmEditorEditorPhases uuid

        QuestionTags ->
            Routes.kmEditorEditorQuestionTags uuid

        Preview ->
            Routes.kmEditorEditorPreview uuid

        Settings ->
            Routes.kmEditorEditorSettings uuid


sameTab : KMEditorRoute -> KMEditorRoute -> Bool
sameTab left right =
    case ( left, right ) of
        ( Edit _, Edit _ ) ->
            True

        _ ->
            left == right


viewTab : AppState -> KMEditorRoute -> Model -> EditorContext -> Html Msg
viewTab appState route model editorContext =
    case route of
        Edit _ ->
            viewEditTab appState model editorContext

        QuestionTags ->
            div [ class "c-km-editor-tab appState" ] [ Tabs.questionTags appState (questionUrl appState model) EventMsg editorContext ]

        Phases ->
            div [ class "c-km-editor-tab appState" ] [ Tabs.phases appState (questionUrl appState model) EventMsg editorContext ]

        Settings ->
            div [ class "c-km-editor-tab appState" ]
                [ Tabs.settings appState
                    { form = model.settingsForm
                    , saving = model.savingSettings
                    , deleting = model.deletingEditor
                    , previousVersion = Maybe.map .version editorContext.kmEditor.previousPackage
                    , formMsg = SettingsFormMsg
                    , onVersion = SettingsSetVersion
                    , onSubmit = SaveSettings
                    , onDelete = SetDeleteEditorOpen True
                    }
                ]

        Preview ->
            case model.preview of
                Just preview ->
                    div [ class "c-km-editor-preview" ]
                        [ viewPreviewBar appState model editorContext
                        , div [ class "c-km-editor-preview__body" ]
                            [ Questionnaire.view appState
                                { wrapMsg = PreviewMsg
                                , readonly = False
                                , commentsReadonly = True
                                , toolbarEnabled = False
                                , actionsEnabled = False
                                , canViewVersions = False
                                , canRevert = False
                                }
                                preview
                            ]
                        ]

                Nothing ->
                    div [ class "c-km-editor-tab appState" ] [ text (gettext "Loading..." appState.locale) ]


{-| Above the preview: which question tags to narrow it by, and the one action
it has.

The values answered here are nobody's project — they are a way of looking at
the knowledge model being written. They are kept only when asked for, and then
they belong to the editor rather than to me: everybody editing it sees them,
and they are what the preview starts from next time.

-}
viewPreviewBar : AppState -> Model -> EditorContext -> Html Msg
viewPreviewBar appState model editorContext =
    let
        tags =
            KnowledgeModel.getTags editorContext.kmEditor.knowledgeModel
                |> List.filterNot (\tag -> EditorContext.isDeleted tag.uuid editorContext)
    in
    div [ class "c-km-editor-preview__bar" ]
        [ if List.isEmpty tags then
            text ""

          else
            div [ class "c-km-editor-preview__tags" ] [ viewPreviewTags appState model tags ]
        , div [ class "c-km-editor-preview__actions" ]
            [ Button.secondary (gettext "Save preview values" appState.locale) SavePreviewReplies
                |> Button.withIcon (Icon.solid "fa-floppy-disk")
                |> Button.withTestId "km-editor_preview_save-values"
                |> Button.toHtml
            ]
        ]


{-| Nothing ticked means every question, not none — the preview opens showing
the whole knowledge model, and a tag narrows it from there. Which is what
"Select none" goes back to, so the pair reads as the range it is, and the line
under the tags says which of the two you are looking at: an empty set of
checkboxes meaning everything is not what a checkbox usually means.
-}
viewPreviewTags : AppState -> Model -> List Tag -> Html Msg
viewPreviewTags appState model tags =
    let
        choice tag =
            Field.choice
                { value = tag.uuid
                , label =
                    if String.isEmpty tag.name then
                        gettext "Untitled question tag" appState.locale

                    else
                        tag.name
                }
                |> Field.withChoiceColor tag.color

        actions =
            div [ class "ui-inline" ]
                [ Button.ghost (gettext "Select all" appState.locale) PreviewSelectAllTags
                    |> Button.withIcon (Icon.solid "fa-check-double")
                    |> Button.withTestId "km-editor_preview_tags_select-all"
                    |> Button.toHtml
                , Button.ghost (gettext "Select none" appState.locale) PreviewSelectNoneTags
                    |> Button.withIcon (Icon.solid "fa-xmark")
                    |> Button.withTestId "km-editor_preview_tags_select-none"
                    |> Button.toHtml
                ]
    in
    Field.checkboxGroup
        { id = "km-editor-preview-tags"
        , label = gettext "Question tags" appState.locale
        , values = model.previewTags
        , options = List.map choice (List.sortBy .name tags)
        , onToggle = PreviewToggleTag
        }
        |> Field.withLabelAction actions
        |> Field.withHint
            (if List.isEmpty model.previewTags then
                gettext "Every question is shown. Tick a tag to see only the questions it is on." appState.locale

             else
                gettext "Only the questions on the ticked tags are shown." appState.locale
            )
        |> Field.toHtml


viewEditTab : AppState -> Model -> EditorContext -> Html Msg
viewEditTab appState model editorContext =
    Workspace.toHtml
        (Workspace.workspace
            { id = workspaceId
            , main = [ Entity.view (entityConfig appState model) editorContext ]
            }
            |> Workspace.withToolbar
                { label = gettext "Knowledge model editor" appState.locale
                , start = [ trail appState model editorContext ]
                , end = List.map (panelButton appState model editorContext) [ Panels.Warnings, Panels.UrlCheck ]
                }
            |> withPanel appState model editorContext
            |> Workspace.withNav
                { label = gettext "Knowledge model" appState.locale
                , header = [ treeActions appState ]
                , content = [ Tree.view (treeConfig appState) editorContext ]
                , resizer = model.navResizer
                , toResizerMsg = NavResizerMsg
                , resizerWords = UiWords.resizer appState (gettext "Resize the knowledge model" appState.locale)
                , overlayOpen = model.navOverlayOpen
                , onOverlay = SetNavOverlay
                , toggleLabel = gettext "Knowledge model" appState.locale
                , closeLabel = gettext "Close the knowledge model" appState.locale
                }
        )


{-| What is about to be published, as it stands.

Publishing is not undoable and the version number it goes out under cannot be
used twice, so the dialog is a last read of what is about to be fixed: the
metadata as it stands and the readme as it will be read. Neither is asked for
again here — they belong to the editor, and the dialog says where to change
them.

-}
viewPublishModal : AppState -> Model -> EditorContext -> Html Msg
viewPublishModal appState model editorContext =
    let
        kmEditor =
            editorContext.kmEditor

        entry term description =
            DescriptionList.entry { term = term, description = description }

        -- A real link rather than a button that navigates, so it survives
        -- middle-click and the screen reader's list of links. Following it
        -- closes the dialog, because every route change in the editor does.
        settingsLink =
            Link.toHtml
                (Link.link
                    { url = Routes.toUrl appState (Routes.kmEditorEditorSettings model.uuid)
                    , label = gettext "Settings" appState.locale
                    }
                )

        readme =
            if String.isEmpty (String.trim kmEditor.readme) then
                []

            else
                [ Card.viewWithTitleLevel 4
                    (gettext "Readme" appState.locale)
                    [ Markdown.view { headingOffset = 4 } kmEditor.readme ]
                ]

        publishError =
            case model.publishing of
                Error message ->
                    [ Alert.toHtml (Alert.danger message) ]

                _ ->
                    []
    in
    Modal.confirm
        { title = gettext "Publish knowledge model" appState.locale
        , open = model.publishOpen
        , onClose = SetPublishOpen False
        , body =
            publishError
                ++ Alert.toHtmlWithContent
                    [ span []
                        (String.formatHtml
                            (gettext "Check the knowledge model's metadata before publishing. You change them in %s." appState.locale)
                            [ settingsLink ]
                        )
                    ]
                    (Alert.info "")
                :: Card.viewWithTitleLevel 4
                    (gettext "Metadata" appState.locale)
                    [ DescriptionList.toHtml
                        [ entry (gettext "Name" appState.locale) kmEditor.name
                        , entry (gettext "Knowledge Model ID" appState.locale) kmEditor.id
                        , entry (gettext "Version" appState.locale) (Version.toString kmEditor.version)
                        , entry (gettext "License" appState.locale) kmEditor.license
                        , entry (gettext "Description" appState.locale) kmEditor.description
                        ]
                    ]
                :: readme
        , confirmLabel = gettext "Publish" appState.locale
        , onConfirm = Publish
        }
        |> Modal.withLoading (ActionResult.isLoading model.publishing)
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> Modal.withHeaderAction (GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.kmEditorPublish)
        |> Modal.withWide
        |> Modal.withTestId "km-editor_publish-modal"
        |> Modal.toHtml


{-| A request written as a cURL command, which is how an API's own
documentation usually gives one. What is parsed out of it replaces the method,
the URL, the headers and — when there is one — the body.
-}
viewCurlImportModal : AppState -> Model -> Html Msg
viewCurlImportModal appState model =
    Modal.plain
        { title = gettext "Import from cURL" appState.locale
        , open = Maybe.isJust model.curlImport
        , onClose = CloseCurlImport
        , body =
            [ Field.toHtml
                (Field.textarea
                    { id = "km-editor-curl"
                    , label = gettext "cURL command" appState.locale
                    , value = Maybe.unwrap "" .curl model.curlImport
                    , onInput = SetCurlString
                    }
                    |> Field.withHint (gettext "Paste the command here. The method, the address, the headers and the body are read out of it." appState.locale)
                )
            ]
        }
        |> Modal.withActions
            [ Button.ghost (gettext "Cancel" appState.locale) CloseCurlImport
            , Button.primary (gettext "Import" appState.locale) ConfirmCurlImport
                |> Button.withTestId "km-editor_curl-import-confirm"
            ]
        |> Modal.withTestId "km-editor_curl-import-modal"
        |> Modal.toHtml


{-| Why the server refused a change, in a dialog rather than instead of the
editor: what is on the screen stays there to be looked at or copied. It is no
longer what the server has, so the way on is a refresh.

The error is a danger alert, so it reads as one at a glance, with what the
server found wrong as a line each under it.

-}
viewSaveErrorModal : AppState -> Model -> Html Msg
viewSaveErrorModal appState model =
    Modal.plain
        { title = gettext "Unable to save" appState.locale
        , open = Maybe.isJust model.saveError
        , onClose = CloseSaveError
        , body =
            case model.saveError of
                Just saveError ->
                    [ Alert.danger ""
                        |> Alert.toHtmlWithContent
                            (Html.p [] [ Html.strong [] [ text saveError.message ] ]
                                :: List.map (\detail -> Html.p [] [ text detail ]) saveError.details
                            )
                    , Html.p [] [ text (gettext "The editor no longer shows what is saved. Refresh the page to continue with the saved knowledge model." appState.locale) ]
                    ]

                Nothing ->
                    []
        }
        |> Modal.withActions
            [ Button.ghost (gettext "Close" appState.locale) CloseSaveError
            , Button.primary (gettext "Refresh" appState.locale) Refresh
                |> Button.withTestId "km-editor_save-error-refresh"
            ]
        |> Modal.withTestId "km-editor_save-error-modal"
        |> Modal.toHtml


{-| Deleting is asked about first. It is soft — the entity stays until the
knowledge model is published — but everything inside it goes with it and there
is no undo, which is more than a button press should decide on its own.
-}
viewDeleteModal : AppState -> Model -> EditorContext -> Html Msg
viewDeleteModal appState model editorContext =
    let
        ( kind, name ) =
            case model.deleteState of
                Just state ->
                    ( state.kind, EditorContext.getEditorName appState.locale state.uuid editorContext )

                Nothing ->
                    ( "", "" )
    in
    ConfirmModal.deleteWithTestId appState
        { title = String.format (gettext "Delete %s" appState.locale) [ kind ]
        , open = Maybe.isJust model.deleteState
        , question = String.format (gettext "Are you sure you want to delete %s?" appState.locale) [ name ]
        , entity = Text.body (gettext "Everything inside it goes with it. It is removed for good when the knowledge model is published." appState.locale)
        , onConfirm = ConfirmDelete
        , onClose = CloseDelete
        , result = ActionResult.Unset
        , testId = Just "km-editor_delete-modal"
        }


{-| Deleting the editor, which is not deleting the knowledge model: everything
done since it was published goes, and the published knowledge model stays. It
cannot be undone, so it is asked about first.
-}
viewDeleteEditorModal : AppState -> Model -> EditorContext -> Html Msg
viewDeleteEditorModal appState model editorContext =
    ConfirmModal.deleteWithTestId appState
        { title = gettext "Delete editor" appState.locale
        , open = model.deleteEditorOpen
        , question =
            String.format (gettext "Are you sure you want to delete the editor of %s?" appState.locale)
                [ editorContext.kmEditor.name ]
        , entity = Text.body (gettext "Every change made in it since the knowledge model was published goes with it. The published knowledge model itself is not touched." appState.locale)
        , onConfirm = DeleteEditor
        , onClose = SetDeleteEditorOpen False
        , result = model.deletingEditor
        , testId = Just "km-editor_delete-editor-modal"
        }


{-| Where the open entity is going. The tree is the knowledge model with
everything that cannot take it disabled, so a target is found by recognizing
where it lives rather than by reading a list of paths.
-}
viewMoveModal : AppState -> Model -> EditorContext -> Html Msg
viewMoveModal appState model editorContext =
    case model.moveState of
        Just state ->
            Modal.plain
                { title = Move.title appState.locale state
                , open = True
                , onClose = CloseMove
                , body =
                    [ div [ class "c-km-editor-move" ]
                        [ div [ class "ui-inline" ]
                            [ Button.toHtml
                                (Button.ghost (gettext "Expand all" appState.locale) MoveExpandAll
                                    |> Button.withIcon (Icon.solid "fa-angles-down")
                                    |> Button.small
                                )
                            , Button.toHtml
                                (Button.ghost (gettext "Collapse all" appState.locale) MoveCollapseAll
                                    |> Button.withIcon (Icon.solid "fa-angles-up")
                                    |> Button.small
                                    |> Button.withTestId "km-editor_move-modal_collapse-all"
                                )
                            ]

                        -- The tree scrolls, the two buttons over it do not: a
                        -- knowledge model is taller than a dialog, and a
                        -- control that scrolls away is a control you have to
                        -- go back for.
                        , div [ class "c-km-editor-move__tree" ]
                            [ Move.tree
                                { locale = appState.locale, onSelect = MoveSelect, onToggle = MoveToggle }
                                editorContext
                                state
                            ]
                        ]
                    ]
                }
                |> Modal.withActions
                    [ Button.ghost (gettext "Cancel" appState.locale) CloseMove
                    , Button.primary (gettext "Move" appState.locale) ConfirmMove
                        |> Button.withDisabled (not (Move.canMove state))
                        |> Button.withTestId "km-editor_move-modal_move"
                    ]
                |> Modal.withTestId "km-editor_move-modal"
                |> Modal.toHtml

        Nothing ->
            text ""


{-| A panel is opened and closed by the same control, which stays pressed while
it is open — the workspace draws no close button of its own beside the work.
-}
panelButton : AppState -> Model -> EditorContext -> Panels.Panel -> Html Msg
panelButton appState model editorContext panel =
    let
        open =
            model.panel == Just panel

        count =
            case panel of
                Panels.Warnings ->
                    Panels.warningCount editorContext

                Panels.UrlCheck ->
                    Panels.brokenCount editorContext model.urlChecker

        withCount =
            if count > 0 then
                Button.withBadge (Badge.danger (String.fromInt count))

            else
                identity
    in
    Button.toHtml
        (Button.ghost (Panels.title (panelsConfig appState model) panel)
            (SetPanel
                (if open then
                    Nothing

                 else
                    Just panel
                )
            )
            |> Button.withPressed open
            |> withCount
            |> Button.small
        )


withPanel : AppState -> Model -> EditorContext -> Workspace.Workspace Msg -> Workspace.Workspace Msg
withPanel appState model editorContext workspace =
    case model.panel of
        Just panel ->
            Workspace.withAside
                { title = Panels.title (panelsConfig appState model) panel
                , content = Panels.view (panelsConfig appState model) editorContext model.urlChecker panel
                , resizer = model.asideResizer
                , toResizerMsg = AsideResizerMsg
                , resizerWords = UiWords.resizer appState (gettext "Resize panel" appState.locale)
                , onClose = SetPanel Nothing
                , closeLabel = gettext "Close panel" appState.locale
                , closeInPanel = False
                }
                workspace

        Nothing ->
            workspace


panelsConfig : AppState -> Model -> Panels.Config Msg
panelsConfig appState model =
    { locale = appState.locale
    , entityUrl = \uuid -> Routes.toUrl appState (Routes.kmEditorEditor model.uuid (entityRoute uuid model))
    , onCheckUrls = CheckUrls
    , onFollowWarning = FollowWarning
    }


workspaceId : String
workspaceId =
    "km-editor"


trailMenuId : String
trailMenuId =
    "km-editor-trail"


{-| Where in the knowledge model the open entity sits, as a trail of links back
to the root.

A knowledge model nests as deep as anyone cares to nest it, and a trail that
long wraps the toolbar; so only the last four places are drawn, and what came
before them is a menu on an ellipsis in their place — the same cut the wizard
makes, except that there the ellipsis is dead text and the way back to the top
of a deep tree is to scroll the navigation and find it.

-}
trail : AppState -> Model -> EditorContext -> Html Msg
trail appState model editorContext =
    let
        path =
            trailPath editorContext

        hidden =
            List.take (List.length path - trailLength) path

        shown =
            List.drop (List.length path - trailLength) path

        label uuid =
            EditorContext.getEditorName appState.locale uuid editorContext

        url uuid =
            Routes.toUrl appState (Routes.kmEditorEditor model.uuid (entityRoute uuid model))

        overflow =
            if List.isEmpty hidden then
                []

            else
                [ Breadcrumbs.withTestId "km-editor_crumb_rest" <|
                    Breadcrumbs.custom
                        (Menu.toHtml
                            { id = trailMenuId
                            , trigger =
                                Menu.custom
                                    { label = gettext "The rest of the path" appState.locale
                                    , class = "ui-breadcrumbs__link"
                                    , content = [ text "…" ]
                                    }
                            , alignment = Menu.Start
                            , items = List.map (\uuid -> Menu.linkItem (label uuid) (url uuid)) hidden
                            , toMsg = TrailMenuMsg (List.length hidden)
                            }
                            model.trailMenu
                        )
                ]

        crumbs =
            List.indexedMap
                (\index uuid ->
                    (if index == List.length shown - 1 then
                        Breadcrumbs.current (label uuid)

                     else
                        Breadcrumbs.crumb { label = label uuid, url = url uuid }
                    )
                        |> Breadcrumbs.withTestId ("km-editor_crumb_" ++ uuid)
                )
                shown
    in
    Breadcrumbs.toHtmlWithWords (UiWords.breadcrumbs appState) (overflow ++ crumbs)


{-| How many places of the trail are drawn before the rest goes into the menu.
-}
trailLength : Int
trailLength =
    4


{-| The open entity and everything it is inside, outermost first.
-}
trailPath : EditorContext -> List String
trailPath editorContext =
    let
        up uuid acc =
            if String.isEmpty uuid then
                acc

            else
                up (EditorContext.getParentUuid uuid editorContext) (uuid :: acc)
    in
    up editorContext.activeUuid []


{-| Opening and closing the whole tree at once. A deep knowledge model is
mostly folded away, and finding one's bearings in it starts with seeing all of
it or none of it; doing that a caret at a time is what these replace.

Two buttons rather than one joined pair: a group says its parts are two halves
of one control — a sort order and its direction — and these are two separate
things to do.

-}
treeActions : AppState -> Html Msg
treeActions appState =
    div [ class "ui-inline" ]
        [ Button.toHtml
            (Button.ghost (gettext "Expand all" appState.locale) ExpandAll
                |> Button.withIcon (Icon.solid "fa-angles-down")
                |> Button.small
                |> Button.withTestId "km-editor_tree_expand-all"
            )
        , Button.toHtml
            (Button.ghost (gettext "Collapse all" appState.locale) CollapseAll
                |> Button.withIcon (Icon.solid "fa-angles-up")
                |> Button.small
                |> Button.withTestId "km-editor_tree_collapse-all"
            )
        ]


treeConfig : AppState -> Tree.Config Msg
treeConfig appState =
    { locale = appState.locale
    , onSelect = SelectEntity
    , onToggle = ToggleNode
    , onAdd = AddEntity
    }


entityConfig : AppState -> Model -> Entity.Config Msg
entityConfig appState model =
    { locale = appState.locale
    , eventMsg = EventMsg
    , entityUrl = \uuid -> Routes.toUrl appState (Routes.kmEditorEditor model.uuid (entityRoute uuid model))
    , secrets = secretNames model
    , maxUploadFileSize = appState.maxUploadFileSize
    , childDrag = model.childDrag
    , onChildDrag = SetChildDrag
    , copy = model.copy
    , copyMsg = CopyMsg
    , onMove = OpenMove
    , onDelete = OpenDelete
    , integration = integrationConfig appState model
    , draft = \key -> Dict.get key model.drafts
    , onDraft = SetDraft
    , onDraftDone = ClearDraft
    }


{-| Everything the integration editor needs of the shell. It is the one form
that runs requests of its own, so it is handed the whole of its configuration
rather than having a dozen more fields on the entity config.
-}
integrationConfig : AppState -> Model -> String -> IntegrationForm.Config Msg
integrationConfig appState model uuid =
    let
        testResult =
            Maybe.withDefault Unset (Dict.get uuid model.integrationTests)

        editorContext =
            ActionResult.toMaybe model.editorContext

        questionsUsing =
            case editorContext of
                Just context ->
                    KnowledgeModel.getAllQuestions context.kmEditor.knowledgeModel
                        |> List.filter (\question -> Question.getIntegrationUuid question == Just uuid)
                        |> List.filterNot (\question -> EditorContext.isDeleted (Question.getUuid question) context)
                        |> List.sortBy Question.getTitle
                        |> List.map
                            (\question ->
                                { uuid = Question.getUuid question
                                , name = EditorContext.getEditorName appState.locale (Question.getUuid question) context
                                , url = Routes.toUrl appState (Routes.kmEditorEditor model.uuid (entityRoute (Question.getUuid question) model))
                                }
                            )

                Nothing ->
                    []
    in
    { locale = appState.locale
    , fieldId = Entity.fieldId uuid
    , uuid = uuid
    , edit = EventMsg True Nothing (parentOf uuid model) (Just uuid)
    , atOnce = EventMsg False Nothing (parentOf uuid model) (Just uuid)
    , secrets = secretNames model
    , secretsUrl = Routes.toUrl appState Routes.knowledgeModelSecrets
    , prefabs = ActionResult.withDefault [] model.integrationPrefabs
    , isEmpty = Maybe.unwrap False (EditorContext.isEmptyIntegrationEditor uuid) editorContext
    , testing = ActionResult.isLoading testResult
    , testError = errorOf testResult
    , onTest = testIntegrationMsg uuid editorContext
    , previewOpen = \name -> Set.member (uuid ++ ":" ++ name) model.templatePreviews
    , preview = \name -> previewOf (uuid ++ ":" ++ name) model
    , onPreview = PreviewTemplate uuid
    , onTemplate = \name -> ShowTemplate (uuid ++ ":" ++ name)
    , cursor = \name -> Dict.get (Entity.fieldId uuid name) model.cursors
    , onCursor = \name -> SetCursor (Entity.fieldId uuid name)
    , onInsert = AfterInsert
    , copy = model.copy
    , copyMsg = CopyMsg
    , onCurlImport = OpenCurlImport uuid
    , questions = questionsUsing
    , plugins =
        AppState.getPluginsByConnector appState .knowledgeModelIntegrations
            |> List.map (\( plugin, connector ) -> ( plugin.uuid, connector ))
    , pluginView = PluginView.view appState
    , editor =
        Maybe.map
            (\context -> { uuid = context.kmEditor.uuid, name = context.kmEditor.name, workspaceUuid = context.kmEditor.workspaceUuid })
            editorContext
    , builtIns = appState.extensions.integrations
    , builtInLoaded = \builtIn -> Maybe.withDefault Loading (Dict.get (BuiltInIntegration.key builtIn) model.builtInLoaded)
    , bootstrapConfig = appState.config
    }


{-| The test uses the query and the variables as they stand on the integration,
which is what the two fields above the button are for.
-}
testIntegrationMsg : String -> Maybe EditorContext -> Msg
testIntegrationMsg uuid mbContext =
    case Maybe.andThen (\context -> KnowledgeModel.getIntegration uuid context.kmEditor.knowledgeModel) mbContext of
        Just (Integration.ApiIntegration data) ->
            TestIntegration uuid data.testQ data.testVariables

        _ ->
            TestIntegration uuid "" Dict.empty


errorOf : ActionResult a -> Maybe String
errorOf result =
    case result of
        Error message ->
            Just message

        _ ->
            Nothing


previewOf : String -> Model -> Maybe (Result String (List TypeHint))
previewOf key model =
    case Dict.get key model.integrationPreviews of
        Just (Success hints) ->
            Just (Ok hints)

        Just (Error message) ->
            Just (Err message)

        _ ->
            Nothing


{-| Where a question's own editor is, for the matrices that list them.
-}
questionUrl : AppState -> Model -> String -> String
questionUrl appState model uuid =
    Routes.toUrl appState (Routes.kmEditorEditor model.uuid (entityRoute uuid model))


parentOf : String -> Model -> String
parentOf uuid model =
    ActionResult.unwrap "" (EditorContext.getParentUuid uuid) model.editorContext


{-| A branch opened or closed in the move dialog's tree, which keeps its own
idea of what is open: looking for somewhere to put something is not the same
journey as editing, and it should not leave the navigation rearranged.
-}
moveSetNodeOpen : String -> Bool -> Move.State -> Move.State
moveSetNodeOpen uuid isOpen state =
    { state
        | openNodes =
            if isOpen then
                Set.insert uuid state.openNodes

            else
                Set.remove uuid state.openNodes
    }


{-| What the page is about, by name, for the window's title, once it has
arrived.
-}
entityName : Model -> Maybe String
entityName model =
    Maybe.map (.kmEditor >> .name) (ActionResult.toMaybe model.editorContext)
