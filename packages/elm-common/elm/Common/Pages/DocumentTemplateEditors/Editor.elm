module Common.Pages.DocumentTemplateEditors.Editor exposing
    ( Dialog
    , Model
    , Msg
    , PreviewMode
    , entityName
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

{-| The document template editor: three tabs over one draft — its files, a
preview of what they make, and its settings.

**There is no websocket here.** Unlike the knowledge model editor and the
project page, this saves over plain HTTP on a button: no autosave, no debounce,
no online users, and no protocol to be faithful to.

**Switching tabs must not reset it.** Everything is held at once and keyed on
the template's uuid: the same template keeps its open files, its unsaved
content and its split widths, and a different template starts fresh. A naive
`init` on every route change silently throws away unsaved edits, which is the
one thing in this shell that is easy to get wrong.

**Everything loads once, up front** — the draft, both prefab lists, the files
and the assets — whichever tab is showing, so the Files model has to tolerate
being filled while it is not on screen. Only the preview is lazy.

**Save is files first, form second.** Every dirty file is `PUT` in parallel;
when the last one lands, and only then, the settings form is saved. A failed
file stays dirty and can be sent again.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Common.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Common.Api.Models.DocumentTemplate.DocumentTemplateAsset exposing (DocumentTemplateAsset)
import Common.Api.Models.DocumentTemplate.DocumentTemplateFile exposing (DocumentTemplateFile)
import Common.Api.Models.DocumentTemplate.DocumentTemplateFormatStep exposing (DocumentTemplateFormatStep)
import Common.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase
import Common.Api.Models.DocumentTemplateDraft.DocumentTemplateDraftPreviewSettings as DocumentTemplateDraftPreviewSettings exposing (DocumentTemplateDraftPreviewSettings)
import Common.Api.Models.DocumentTemplateDraft.DocumentTemplateFormatDraft exposing (DocumentTemplateFormatDraft)
import Common.Api.Models.DocumentTemplateDraftDetail as DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Common.Api.Models.KnowledgeModelEditorSuggestion exposing (KnowledgeModelEditorSuggestion)
import Common.Api.Models.Prefab exposing (Prefab)
import Common.Api.Models.ProjectSuggestion exposing (ProjectSuggestion)
import Common.Api.Models.UrlResponse exposing (UrlResponse)
import Common.Api.Prefabs as PrefabsApi
import Common.Api.Projects as ProjectsApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.DetailPage as DetailPage
import Common.Components.DocumentPreviewError as DocumentPreviewError
import Common.Components.Form as CommonForm
import Common.Components.GuideLink as GuideLink
import Common.Components.Markdown as Markdown
import Common.Components.Page as Page
import Common.Components.TypeHint as TypeHint
import Common.Components.UiWords as UiWords
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.GuideLinks as GuideLinks
import Common.Data.NotFound as NotFound
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.DocumentTemplateEditors.Editor.Files as Files
import Common.Pages.DocumentTemplateEditors.Editor.Groups as Groups exposing (Groups)
import Common.Pages.DocumentTemplateEditors.Editor.Routes exposing (DTEditorRoute(..), DTSettingsSection(..))
import Common.Pages.DocumentTemplateEditors.Editor.Settings as Settings
import Common.Ports.Dom as Dom
import Common.Ports.Window as Window
import Common.Routes as Routes
import Common.Utils.ContentType as ContentType
import Common.Utils.RequestHelpers as RequestHelpers
import Dict exposing (Dict)
import File
import Gettext exposing (gettext)
import Html exposing (Html, button, div, iframe, img, span, strong, text)
import Html.Attributes exposing (alt, attribute, class, classList, src, title, type_)
import Html.Events exposing (onClick)
import Http
import List.Extra as List
import Maybe.Extra as Maybe
import Process
import Random exposing (Seed)
import Set exposing (Set)
import String.Format as String
import Task
import Ui.Alert as Alert
import Ui.Button as Button
import Ui.Card as Card
import Ui.DescriptionList as DescriptionList
import Ui.Field as Field
import Ui.FileUpload as FileUpload
import Ui.Icon as Icon
import Ui.Illustration as Illustration
import Ui.Link as Link
import Ui.Menu as Menu
import Ui.Modal as Modal
import Ui.ModeSwitch as ModeSwitch
import Ui.Page.Content as Content
import Ui.Page.Message as Message
import Ui.Page.SplitNav as SplitNav
import Ui.Page.Workspace as Workspace
import Ui.Resizer as Resizer
import Ui.TestId as TestId
import Ui.Text as Text
import Ui.Tooltip as Tooltip
import Uuid exposing (Uuid)
import Uuid.Extra as Uuid
import Version



-- MODEL


type alias Model =
    { uuid : Uuid

    -- Which tab is open, because the preview is asked for on the way into its
    -- own tab and nowhere else — and on a cold load the draft it needs arrives
    -- after the route does.
    , route : DTEditorRoute
    , draft : ActionResult DocumentTemplateDraftDetail
    , files : ActionResult (List DocumentTemplateFile)
    , assets : ActionResult (List DocumentTemplateAsset)
    , formatPrefabs : ActionResult (List DocumentTemplateFormatDraft)
    , formatStepPrefabs : ActionResult (List DocumentTemplateFormatStep)

    -- What has been typed and not yet sent: the content of every file that has
    -- been opened, and the uuids of those that differ from the server's copy.
    , contents : Dict String (ActionResult String)
    , dirty : List String
    , savingFiles : Dict String (ActionResult ())

    -- Why the last save did not go through. The draft itself is never replaced
    -- by an error: everything typed since the last save lives in it, and a
    -- failed request is a reason to try again rather than to lose the work.
    , saveError : Maybe String

    -- Whether the settings PUT is on its way. The files' own requests are in
    -- `savingFiles`, so a save is in progress while either says so.
    , savingDraft : Bool
    , discardOpen : Bool

    -- Publishing: the last look at what is about to be fixed, and the request
    -- that fixes it.
    , publishOpen : Bool
    , publishing : ActionResult String

    -- What is open in the middle, on one side or on two, and which folders are
    -- folded away.
    , groups : Groups
    , wrap : Bool

    -- An asset's URL is presigned and expires, so it is fetched fresh when the
    -- asset is opened rather than taken from the listing. Two can be open at
    -- once, one on each side.
    , assetCache : Dict String (ActionResult DocumentTemplateAsset)
    , collapsed : Set String
    , navResizer : Resizer.Model

    -- The file list on a narrow page, where it is an overlay opened from the
    -- toolbar rather than a column beside the files.
    , navOverlayOpen : Bool

    -- The settings are the draft itself; this says whether it differs from the
    -- server's copy. The revision counts the changes to it, so a save that
    -- comes back can tell whether anything was changed while it was away.
    , settingsDirty : Bool
    , settingsRevision : Int

    -- What the preview is rendered against: a project or a knowledge model
    -- editor, and the search for one of each. Which of the two is being chosen
    -- is the client's; what was chosen is on the draft.
    , previewMode : PreviewMode
    , projectTypeHint : TypeHint.Model ProjectSuggestion
    , kmEditorTypeHint : TypeHint.Model KnowledgeModelEditorSuggestion

    -- The preview is lazy: it is asked for when the tab is entered, and again
    -- when the format or the source changes.
    , selected : Files.Selection
    , newFolders : List String

    -- Which dialog is open, and what has been typed into it.
    , dialog : Dialog
    , dialogName : String
    , addMenu : Menu.Model

    -- Where a move is going, once a folder has been chosen, and which folders
    -- of the tree it is chosen from are open.
    , moveTarget : Maybe String
    , moveExpanded : Set String
    , dialogResult : ActionResult String
    , upload : FileUpload.Model
    , preview : ActionResult UrlResponse
    , previewUnsupported : Maybe String

    -- The template engine's own words when the preview failed, set together
    -- with the preview's error.
    , previewLog : Maybe String
    }


initialModel : AppState -> Uuid -> Model
initialModel appState uuid =
    { uuid = uuid
    , route = Files
    , draft = Loading
    , files = Loading
    , assets = Loading
    , formatPrefabs = Loading
    , formatStepPrefabs = Loading
    , contents = Dict.empty
    , dirty = []
    , savingFiles = Dict.empty
    , saveError = Nothing
    , savingDraft = False
    , discardOpen = False
    , publishOpen = False
    , publishing = Unset
    , groups = Groups.init
    , wrap = False
    , assetCache = Dict.empty
    , collapsed = Set.empty
    , navResizer = Resizer.init (navResizerConfig appState)
    , navOverlayOpen = False
    , settingsDirty = False
    , settingsRevision = 0
    , previewMode = ProjectPreview
    , projectTypeHint = TypeHint.init
    , kmEditorTypeHint = TypeHint.init
    , selected = Files.NoSelection
    , newFolders = []
    , dialog = NoDialog
    , dialogName = ""
    , addMenu = Menu.init
    , moveTarget = Nothing
    , moveExpanded = Set.empty
    , dialogResult = Unset
    , upload = FileUpload.init
    , preview = Unset
    , previewUnsupported = Nothing
    , previewLog = Nothing
    }


navResizerConfig : AppState -> Resizer.Config
navResizerConfig appState =
    { edge = Resizer.Trailing
    , label = gettext "Resize the file list" appState.locale
    , min = 200
    , max = 560
    , default = 280
    , collapse = Nothing
    }


init : AppState -> Uuid -> DTEditorRoute -> Model -> ( Model, Cmd Msg )
init appState uuid route model =
    if model.uuid == uuid && not (ActionResult.isUnset model.draft) then
        -- The same template, another tab: everything stays, including what has
        -- been typed and not saved.
        withPreview appState { model | route = route }

    else
        let
            fresh =
                initialModel appState uuid
        in
        ( { fresh | route = route }, fetch appState uuid )


{-| The preview is asked for on the way into its own tab, and only when the
draft says what it is made from and there is nothing on screen already.

On a cold load the route arrives before the draft does, so this is asked twice:
when the tab is entered and when the draft lands.

-}
withPreview : AppState -> Model -> ( Model, Cmd Msg )
withPreview appState model =
    case ( model.route, model.draft ) of
        ( Preview, Success draft ) ->
            if DocumentTemplateDraftDetail.isPreviewSet draft && ActionResult.isUnset model.preview then
                ( { model | preview = Loading }
                , DocumentTemplateDraftsApi.getPreview appState model.uuid GotPreview
                )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


fetch : AppState -> Uuid -> Cmd Msg
fetch appState uuid =
    Cmd.batch
        [ DocumentTemplateDraftsApi.getDraft appState uuid GotDraft
        , DocumentTemplateDraftsApi.getFiles appState uuid GotFiles
        , DocumentTemplateDraftsApi.getAssets appState uuid GotAssets
        , PrefabsApi.getDocumentTemplateFormatPrefabs appState GotFormatPrefabs
        , PrefabsApi.getDocumentTemplateFormatStepPrefabs appState GotFormatStepPrefabs
        ]


{-| Anything typed and not sent is a reason not to leave — but moving between
the three tabs of the same template is not leaving.
-}
isGuarded : AppState -> Routes.Route -> Model -> Maybe String
isGuarded appState nextRoute model =
    if not (hasChanges model) then
        Nothing

    else if Routes.isDocumentTemplateEditor model.uuid nextRoute then
        Nothing

    else
        Just (gettext "There are unsaved changes." appState.locale)


hasChanges : Model -> Bool
hasChanges model =
    not (List.isEmpty model.dirty) || model.settingsDirty


{-| A save is on its way until every file has answered, and the settings after
them. A file that failed has answered: the save ends there, with its error.
-}
isSaving : Model -> Bool
isSaving model =
    model.savingDraft || List.any ActionResult.isLoading (Dict.values model.savingFiles)



-- UPDATE


{-| What the toolbar has opened. Renaming and moving both end in the same three
requests with a different path, so they are one branch with a different name in
it.
-}
type Dialog
    = NoDialog
    | AddFileDialog
    | AddFolderDialog
    | RenameDialog
    | MoveDialog
    | DeleteDialog
    | UploadDialog


{-| Which of the two things a preview can be rendered against is being chosen.
-}
type PreviewMode
    = ProjectPreview
    | KMEditorPreview


type Msg
    = GotDraft (Result ApiError DocumentTemplateDraftDetail)
    | GotFiles (Result ApiError (List DocumentTemplateFile))
    | GotAssets (Result ApiError (List DocumentTemplateAsset))
    | GotFormatPrefabs (Result ApiError (List (Prefab DocumentTemplateFormatDraft)))
    | GotFormatStepPrefabs (Result ApiError (List (Prefab DocumentTemplateFormatStep)))
    | Save
    | SavedFile String String (Result ApiError ())
    | CloseSaveError
    | SetDiscardOpen Bool
    | SetPublishOpen Bool
    | Publish
    | Published (Result ApiError DocumentTemplateDraftDetail)
    | Discard
    | Open Files.Entry
    | Activate Groups.Side Files.Entry
    | CloseTab Groups.Side Files.Entry
    | MoveToOtherSide Groups.Side
    | ToggleWrap
    | GotFileContent String (Result ApiError String)
    | GotAsset String (Result ApiError DocumentTemplateAsset)
    | FileChanged String String
    | ToggleFolder String Bool
    | NavResizerMsg Resizer.Msg
    | SetNavOverlay Bool
    | SetDraft DocumentTemplateDraftDetail
    | SavedDraft Int (Result ApiError DocumentTemplateDraftDetail)
    | AddFormat
    | LoadPreview
    | SavedPreviewSettings (Result ApiError DocumentTemplateDraftPreviewSettings)
    | GotPreview (Result ApiError ( Http.Metadata, Maybe UrlResponse ))
    | SetPreviewFormat String
    | SetPreviewMode PreviewMode
    | ProjectTypeHintMsg (TypeHint.Msg ProjectSuggestion)
    | KMEditorTypeHintMsg (TypeHint.Msg KnowledgeModelEditorSuggestion)
    | SetPreviewProject (Maybe ProjectSuggestion)
    | SetPreviewKMEditor (Maybe KnowledgeModelEditorSuggestion)
    | Select Files.Selection
    | OpenDialog Dialog
    | CloseDialog
    | SetDialogName String
    | AddMenuMsg Menu.Msg
    | MoveSelect String
    | MoveToggle String Bool
    | ConfirmDialog
    | AddedFile String (Result ApiError DocumentTemplateFile)
    | RenamedFile (Result ApiError ())
    | MovedFolder String String (Result ApiError ())
    | DeletedFolder String (Result ApiError ())
    | Deleted (Result ApiError ())
    | UploadMsg FileUpload.Msg
    | UploadedAsset (Result ApiError DocumentTemplateAsset)
    | GotUploadContent String String
    | GotFileContentForRename String String (Result ApiError String)


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Seed, Model, Cmd msg )
update cfg appState msg model =
    let
        withSeed ( newModel, cmd ) =
            ( appState.seed, newModel, cmd )

        plain ( newModel, cmd ) =
            ( appState.seed, newModel, Cmd.map cfg.wrapMsg cmd )

        quiet newModel =
            ( appState.seed, newModel, Cmd.none )
    in
    case msg of
        GotDraft result ->
            let
                ( withDraft, seed, resultCmd ) =
                    RequestHelpers.applyResultOrNotFound
                        { setResult = \value m -> withPreviewSource value { m | draft = value }
                        , defaultError = gettext "Unable to get the document template." appState.locale
                        , notFoundCmd = UpdateConfig.notFound cfg (NotFound.documentTemplateEditor appState)
                        , notAllowedCmd = UpdateConfig.notAllowed cfg
                        , model = model
                        , result = result
                        , logoutMsg = cfg.logoutMsg
                        , locale = appState.locale
                        }
                        |> (\( m, c ) -> ( m, appState.seed, c ))

                ( loaded, previewCmd ) =
                    withPreview appState withDraft

                workspaceCmd =
                    case result of
                        Ok draft ->
                            UpdateConfig.entityWorkspace cfg draft.workspaceUuid

                        Err _ ->
                            Cmd.none
            in
            ( seed, loaded, Cmd.batch [ resultCmd, Cmd.map cfg.wrapMsg previewCmd, workspaceCmd ] )

        GotFiles result ->
            withSeed <|
                RequestHelpers.applyResult
                    { setResult = \value m -> pruneTabs { m | files = value }
                    , defaultError = gettext "Unable to get the files." appState.locale
                    , model = model
                    , result = result
                    , logoutMsg = cfg.logoutMsg
                    , locale = appState.locale
                    }

        GotAssets result ->
            withSeed <|
                RequestHelpers.applyResult
                    { setResult = \value m -> pruneTabs { m | assets = value }
                    , defaultError = gettext "Unable to get the assets." appState.locale
                    , model = model
                    , result = result
                    , logoutMsg = cfg.logoutMsg
                    , locale = appState.locale
                    }

        GotFormatPrefabs result ->
            -- The prefabs are a convenience in the Settings tab; failing to
            -- load them is not an error the editor shows.
            quiet { model | formatPrefabs = prefabResult result }

        GotFormatStepPrefabs result ->
            quiet { model | formatStepPrefabs = prefabResult result }

        Save ->
            if isSaving model then
                -- Cmd/Ctrl+Enter does not wait for the button to be enabled.
                quiet model

            else if List.isEmpty model.dirty then
                plain
                    ( { model | savingDraft = True, savingFiles = Dict.empty, saveError = Nothing }
                    , saveDraft appState model
                    )

            else
                plain
                    ( { model
                        | savingFiles = Dict.fromList (List.map (\fileUuid -> ( fileUuid, Loading )) model.dirty)
                        , saveError = Nothing
                      }
                    , saveDirtyFiles appState model
                    )

        SavedFile fileUuid sent result ->
            case result of
                Ok _ ->
                    let
                        -- Typing goes on while a save is away. The file is
                        -- clean only if what is in it now is what was sent.
                        unchanged =
                            Dict.get fileUuid model.contents == Just (Success sent)

                        saved =
                            { model
                                | dirty =
                                    if unchanged then
                                        List.filter ((/=) fileUuid) model.dirty

                                    else
                                        model.dirty
                                , savingFiles = Dict.insert fileUuid (Success ()) model.savingFiles
                            }

                        answers =
                            Dict.values saved.savingFiles
                    in
                    -- The settings PUT waits for the last file, so it happens
                    -- once rather than once per file (§3.1), and not at all
                    -- when a file failed.
                    if List.any ActionResult.isLoading answers || List.any ActionResult.isError answers then
                        quiet saved

                    else if saved.settingsDirty then
                        plain ( { saved | savingDraft = True }, saveDraft appState saved )

                    else
                        withSeed ( saved, clearUnloadMessageIfSaved saved )

                Err error ->
                    -- The file stays dirty, so Save can be pressed again.
                    withSeed <|
                        ( { model
                            | savingFiles =
                                Dict.insert fileUuid
                                    (ApiError.toActionResult appState (gettext "Unable to save the file." appState.locale) error)
                                    model.savingFiles
                          }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )

        CloseSaveError ->
            quiet
                { model
                    | saveError = Nothing
                    , savingFiles = Dict.filter (\_ answer -> not (ActionResult.isError answer)) model.savingFiles
                }

        SetDiscardOpen open ->
            quiet { model | discardOpen = open }

        SetPublishOpen open ->
            quiet { model | publishOpen = open, publishing = Unset }

        Publish ->
            case model.draft of
                Success draft ->
                    plain
                        ( { model | publishing = Loading }
                        , DocumentTemplateDraftsApi.putDraft appState
                            model.uuid
                            (Settings.encode (DocumentTemplatePhase.toString DocumentTemplatePhase.Released) draft)
                            Published
                        )

                _ ->
                    quiet model

        Published result ->
            case result of
                Ok draft ->
                    -- The draft is a published template now, and its own page
                    -- is where it is read.
                    ( appState.seed
                    , model
                    , Routes.navigate appState (Routes.documentTemplatesDetail draft.uuid)
                    )

                Err error ->
                    withSeed
                        ( { model | publishing = ApiError.toActionResult appState (gettext "Unable to publish the document template." appState.locale) error }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )

        Discard ->
            plain
                ( initialModel appState model.uuid
                , Cmd.batch [ fetch appState model.uuid, Window.clearUnloadMessage () ]
                )

        Open entry ->
            let
                ( opened, openCmd ) =
                    openEntry appState { model | groups = Groups.open entry model.groups, navOverlayOpen = False } entry
            in
            -- On a narrow page the file list is an overlay over the file it has
            -- just opened, so it gets out of the way, and focus goes to what was
            -- opened rather than with the list.
            plain
                ( opened
                , if model.navOverlayOpen then
                    Cmd.batch [ openCmd, Dom.focus ("#" ++ openedId entry) ]

                  else
                    openCmd
                )

        Activate side entry ->
            quiet { model | groups = Groups.activate side entry model.groups, selected = Files.SelectedEntry entry }

        CloseTab side entry ->
            quiet { model | groups = Groups.close side entry model.groups }

        MoveToOtherSide side ->
            quiet { model | groups = Groups.moveToOtherSide side model.groups }

        ToggleWrap ->
            quiet { model | wrap = not model.wrap }

        GotFileContent fileUuid result ->
            case result of
                Ok content ->
                    quiet { model | contents = Dict.insert fileUuid (Success content) model.contents }

                Err error ->
                    withSeed <|
                        ( { model
                            | contents =
                                Dict.insert fileUuid
                                    (ApiError.toActionResult appState (gettext "Unable to get the file." appState.locale) error)
                                    model.contents
                          }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )

        GotAsset assetUuid result ->
            withSeed <|
                RequestHelpers.applyResult
                    { setResult = \value m -> { m | assetCache = Dict.insert assetUuid value m.assetCache }
                    , defaultError = gettext "Unable to get the asset." appState.locale
                    , model = model
                    , result = result
                    , logoutMsg = cfg.logoutMsg
                    , locale = appState.locale
                    }

        FileChanged fileUuid content ->
            withSeed <|
                ( { model
                    | contents = Dict.insert fileUuid (Success content) model.contents
                    , dirty =
                        if List.member fileUuid model.dirty then
                            model.dirty

                        else
                            fileUuid :: model.dirty
                  }
                , Window.setUnloadMessage (gettext "There are unsaved changes." appState.locale)
                )

        ToggleFolder path open ->
            quiet
                { model
                    | collapsed =
                        if open then
                            Set.remove path model.collapsed

                        else
                            Set.insert path model.collapsed
                }

        NavResizerMsg resizerMsg ->
            quiet { model | navResizer = Resizer.update resizerMsg model.navResizer }

        SetNavOverlay isOpen ->
            -- Focus follows the overlay: into it when it opens, back to the
            -- control that opened it when it closes.
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

        LoadPreview ->
            plain ( { model | preview = Loading, previewUnsupported = Nothing }, DocumentTemplateDraftsApi.getPreview appState model.uuid GotPreview )

        SavedPreviewSettings result ->
            -- Asked for only once the settings say what to render and in
            -- which format: half of them is nothing the server can make.
            case result of
                Ok settings ->
                    if DocumentTemplateDraftPreviewSettings.isPreviewSet settings then
                        update cfg appState LoadPreview model

                    else
                        quiet model

                Err _ ->
                    withSeed ( model, RequestHelpers.getResultCmd cfg.logoutMsg result )

        GotPreview result ->
            case result of
                Ok ( metadata, mbUrlResponse ) ->
                    case mbUrlResponse of
                        Just urlResponse ->
                            quiet { model | preview = Success urlResponse }

                        Nothing ->
                            if metadata.statusCode == 202 then
                                -- Still being rendered. The server says so with
                                -- a 202 and no body, so the page asks again in a
                                -- moment rather than reading the silence as a
                                -- format it cannot show.
                                plain ( { model | preview = Loading }, Task.perform (always LoadPreview) (Process.sleep 1000) )

                            else
                                -- The server answered with the document itself
                                -- rather than a URL: the browser cannot show
                                -- this format, so what is offered is a download.
                                quiet
                                    { model
                                        | preview = Unset
                                        , previewUnsupported = Just (Maybe.withDefault "" (Dict.get "content-type" metadata.headers))
                                    }

                Err error ->
                    let
                        previewError =
                            DocumentPreviewError.fromApiError appState (gettext "Unable to get the preview." appState.locale) error
                    in
                    withSeed <|
                        ( { model | preview = Error previewError.message, previewLog = previewError.log }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )

        SetPreviewFormat value ->
            plain
                (savePreviewSettings appState
                    model
                    (\settings ->
                        { settings
                            | formatUuid =
                                if String.isEmpty value then
                                    Nothing

                                else
                                    Just (Uuid.fromUuidString value)
                        }
                    )
                )

        SetPreviewMode mode ->
            quiet { model | previewMode = mode }

        ProjectTypeHintMsg typeHintMsg ->
            let
                ( projectTypeHint, cmd ) =
                    TypeHint.update appState
                        { wrapMsg = cfg.wrapMsg << ProjectTypeHintMsg
                        , getTypeHints = ProjectsApi.getSuggestions appState PaginationQueryFilters.empty
                        , getError = gettext "Unable to get projects." appState.locale
                        , setReply = cfg.wrapMsg << SetPreviewProject << Just
                        , clearReply = Just (cfg.wrapMsg (SetPreviewProject Nothing))
                        , filterResults = Nothing
                        , logoutMsg = cfg.logoutMsg
                        , id = previewProjectId
                        , toKey = Uuid.toString << .uuid
                        }
                        typeHintMsg
                        model.projectTypeHint
            in
            ( appState.seed, { model | projectTypeHint = projectTypeHint }, cmd )

        KMEditorTypeHintMsg typeHintMsg ->
            let
                ( kmEditorTypeHint, cmd ) =
                    TypeHint.update appState
                        { wrapMsg = cfg.wrapMsg << KMEditorTypeHintMsg
                        , getTypeHints = KnowledgeModelEditorsApi.getKnowledgeModelEditorSuggestions appState
                        , getError = gettext "Unable to get knowledge model editors." appState.locale
                        , setReply = cfg.wrapMsg << SetPreviewKMEditor << Just
                        , clearReply = Just (cfg.wrapMsg (SetPreviewKMEditor Nothing))
                        , filterResults = Nothing
                        , logoutMsg = cfg.logoutMsg
                        , id = previewKMEditorId
                        , toKey = Uuid.toString << .uuid
                        }
                        typeHintMsg
                        model.kmEditorTypeHint
            in
            ( appState.seed, { model | kmEditorTypeHint = kmEditorTypeHint }, cmd )

        SetPreviewProject project ->
            -- One source at a time: choosing a project is choosing not to
            -- render against a knowledge model editor.
            plain
                (savePreviewSettings appState
                    model
                    (\settings ->
                        { settings
                            | projectUuid = Maybe.map .uuid project
                            , project = project
                            , knowledgeModelEditorUuid = Nothing
                            , knowledgeModelEditor = Nothing
                        }
                    )
                )

        SetPreviewKMEditor kmEditor ->
            plain
                (savePreviewSettings appState
                    model
                    (\settings ->
                        { settings
                            | knowledgeModelEditorUuid = Maybe.map .uuid kmEditor
                            , knowledgeModelEditor = kmEditor
                            , projectUuid = Nothing
                            , project = Nothing
                        }
                    )
                )

        Select selection ->
            quiet { model | selected = selection }

        OpenDialog dialog ->
            quiet
                { model
                    | dialog = dialog
                    , dialogResult = Unset
                    , dialogName =
                        case dialog of
                            RenameDialog ->
                                Files.nameOf (selectedName model)

                            MoveDialog ->
                                Files.parentOf (selectedName model)

                            _ ->
                                ""
                    , moveTarget = Nothing

                    -- Where it is now, in the open, so it can be seen in its
                    -- place before it is taken out of it.
                    , moveExpanded = ancestorsOf (Files.parentOf (selectedName model))
                }

        CloseDialog ->
            quiet { model | dialog = NoDialog, dialogName = "", upload = FileUpload.init }

        SetDialogName name ->
            quiet { model | dialogName = name }

        AddMenuMsg menuMsg ->
            let
                ( addMenu, menuCmd ) =
                    Menu.update addMenuId addMenuItems menuMsg model.addMenu
            in
            plain ( { model | addMenu = addMenu }, Cmd.map AddMenuMsg menuCmd )

        MoveSelect path ->
            quiet { model | moveTarget = Just path }

        MoveToggle path expanded ->
            quiet
                { model
                    | moveExpanded =
                        if expanded then
                            Set.insert path model.moveExpanded

                        else
                            Set.remove path model.moveExpanded
                }

        ConfirmDialog ->
            case ( model.dialog, model.selected ) of
                ( AddFolderDialog, _ ) ->
                    let
                        path =
                            into (Files.pathOf (filesConfigOf model) model.selected) model.dialogName
                    in
                    -- Selected, because a folder is made in order to put
                    -- something in it: the next Add goes there without anybody
                    -- having to point at it first.
                    quiet
                        { model
                            | newFolders = model.newFolders ++ [ path ]
                            , selected = Files.SelectedFolder path
                            , dialog = NoDialog
                            , dialogName = ""
                        }

                ( RenameDialog, Files.SelectedFolder path ) ->
                    plain (moveFolderTo appState model path (into (Files.parentOf path) model.dialogName))

                ( MoveDialog, Files.SelectedFolder path ) ->
                    plain (moveFolderTo appState model path (into (Maybe.withDefault "" model.moveTarget) (Files.nameOf path)))

                ( DeleteDialog, Files.SelectedFolder path ) ->
                    if isClientFolder model path then
                        -- Nothing to ask the server: this folder is a name the
                        -- client is holding, and dropping the name is the whole
                        -- of the delete.
                        quiet
                            { model
                                | newFolders = List.filterNot (Files.isUnder path) model.newFolders
                                , selected = Files.NoSelection
                                , dialog = NoDialog
                                , dialogResult = Unset
                            }

                    else
                        plain ( { model | dialogResult = Loading }, confirmDialog appState model )

                _ ->
                    plain ( { model | dialogResult = Loading }, confirmDialog appState model )

        DeletedFolder path result ->
            case result of
                Ok _ ->
                    afterChange cfg
                        appState
                        { model
                            | newFolders = List.filterNot (Files.isUnder path) model.newFolders
                            , selected = Files.NoSelection
                        }
                        result

                Err _ ->
                    afterChange cfg appState model result

        MovedFolder from to result ->
            case result of
                Ok _ ->
                    afterChange cfg
                        appState
                        { model
                            | newFolders = List.map (Files.renamePath from to) model.newFolders
                            , selected = Files.SelectedFolder to
                        }
                        result

                Err _ ->
                    afterChange cfg appState model result

        GotFileContentForRename uuid newPath result ->
            case result of
                Ok content ->
                    plain
                        ( { model | contents = Dict.insert uuid (Success content) model.contents }
                        , DocumentTemplateDraftsApi.putFile appState
                            model.uuid
                            { uuid = Uuid.fromUuidString uuid, fileName = newPath }
                            content
                            RenamedFile
                        )

                Err error ->
                    withSeed ( { model | dialogResult = dialogError appState error }, RequestHelpers.getResultCmd cfg.logoutMsg result )

        AddedFile content result ->
            case result of
                Ok file ->
                    plain
                        ( { model
                            | files = ActionResult.map (\files -> files ++ [ file ]) model.files

                            -- What it was made with: nothing for a new file, and
                            -- what was read for an upload. The editor shows this
                            -- and a save sends it back, so anything else here
                            -- would replace the file on the server.
                            , contents = Dict.insert (Uuid.toString file.uuid) (Success content) model.contents
                            , groups = Groups.open (Files.File (Uuid.toString file.uuid)) model.groups
                            , selected = Files.SelectedEntry (Files.File (Uuid.toString file.uuid))
                            , dialog = NoDialog
                            , dialogResult = Unset
                            , upload = FileUpload.init
                          }
                          -- The file was made to be written in, and the dialog
                          -- that made it has taken the focus with it. The port
                          -- waits for the element, which the code editor builds a
                          -- frame after this.
                        , Dom.focus ("#" ++ fileEditorId (Uuid.toString file.uuid))
                        )

                Err error ->
                    withSeed ( { model | dialogResult = dialogError appState error }, RequestHelpers.getResultCmd cfg.logoutMsg result )

        RenamedFile result ->
            afterChange cfg appState model result

        Deleted result ->
            afterChange cfg appState { model | selected = Files.NoSelection } result

        UploadMsg uploadMsg ->
            let
                ( upload, uploadCmd ) =
                    FileUpload.update { mimes = [ "*/*" ], read = False } uploadMsg model.upload
            in
            plain ( { model | upload = upload }, Cmd.map UploadMsg uploadCmd )

        UploadedAsset result ->
            case result of
                Ok asset ->
                    quiet
                        { model
                            | assets = ActionResult.map (\assets -> assets ++ [ asset ]) model.assets
                            , dialog = NoDialog
                            , dialogResult = Unset
                            , upload = FileUpload.init
                        }

                Err error ->
                    withSeed ( { model | dialogResult = dialogError appState error }, RequestHelpers.getResultCmd cfg.logoutMsg result )

        GotUploadContent path content ->
            -- An uploaded text file is added as a file, which is then opened
            -- like one made here (`AddedFile`).
            plain
                ( model
                , DocumentTemplateDraftsApi.postFile appState
                    model.uuid
                    { uuid = Uuid.nil, fileName = path }
                    content
                    (AddedFile content)
                )

        AddFormat ->
            case model.draft of
                Success draft ->
                    let
                        ( uuid, seed ) =
                            Uuid.step appState.seed

                        withFormat =
                            { draft | formats = draft.formats ++ [ Settings.newFormat (ActionResult.withDefault [] model.formatPrefabs) uuid ] }
                    in
                    ( seed
                    , { model | draft = Success withFormat, settingsDirty = True, settingsRevision = model.settingsRevision + 1 }
                    , Window.setUnloadMessage (gettext "There are unsaved changes." appState.locale)
                    )

                _ ->
                    quiet model

        SetDraft draft ->
            withSeed <|
                ( { model | draft = Success draft, settingsDirty = True, settingsRevision = model.settingsRevision + 1 }
                , Window.setUnloadMessage (gettext "There are unsaved changes." appState.locale)
                )

        SavedDraft revision result ->
            case result of
                Ok draft ->
                    let
                        -- What was changed while the save was away stays, and
                        -- stays unsaved.
                        saved =
                            if revision == model.settingsRevision then
                                { model | draft = Success draft, settingsDirty = False, savingDraft = False }

                            else
                                { model | savingDraft = False }
                    in
                    withSeed ( saved, clearUnloadMessageIfSaved saved )

                Err error ->
                    withSeed <|
                        ( { model
                            | saveError =
                                failureOf (ApiError.toActionResult appState (gettext "Unable to save the document template." appState.locale) error)
                            , savingDraft = False
                          }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )


{-| The guard on leaving goes once a save has left nothing behind it.
-}
clearUnloadMessageIfSaved : Model -> Cmd msg
clearUnloadMessageIfSaved model =
    if hasChanges model then
        Cmd.none

    else
        Window.clearUnloadMessage ()


{-| The words an errored result carries, which is what the page shows. Nothing
at all from a result that did not fail.
-}
failureOf : ActionResult a -> Maybe String
failureOf result =
    case result of
        Error message ->
            Just message

        _ ->
            Nothing


prefabResult : Result ApiError (List (Prefab a)) -> ActionResult (List a)
prefabResult result =
    case result of
        Ok prefabs ->
            Success (List.map .content prefabs)

        Err _ ->
            Error ""


{-| Fetching what an entry needs to be shown, if it is not already held.

A file's content is cached for the session, so reopening a file is instant and
anything typed into it survives closing it. An asset's URL is not: it is
presigned and expires, so it is asked for each time the asset is opened.

-}
openEntry : AppState -> Model -> Files.Entry -> ( Model, Cmd Msg )
openEntry appState model entry =
    let
        selected =
            { model | selected = Files.SelectedEntry entry }
    in
    case entry of
        Files.File fileUuid ->
            if Dict.member fileUuid model.contents then
                ( selected, Cmd.none )

            else
                ( { selected | contents = Dict.insert fileUuid Loading model.contents }
                , DocumentTemplateDraftsApi.getFileContent appState
                    model.uuid
                    (Uuid.fromUuidString fileUuid)
                    (GotFileContent fileUuid)
                )

        Files.Asset assetUuid ->
            ( { selected | assetCache = Dict.insert assetUuid Loading model.assetCache }
            , DocumentTemplateDraftsApi.getAsset appState model.uuid (Uuid.fromUuidString assetUuid) (GotAsset assetUuid)
            )


{-| Tabs on files that are no longer there. A delete takes a file away while it
is open, and the tab of a file the server no longer has is a tab onto nothing.
-}
pruneTabs : Model -> Model
pruneTabs model =
    let
        existing =
            Set.fromList
                (List.map (Uuid.toString << .uuid) (ActionResult.withDefault [] model.files)
                    ++ List.map (Uuid.toString << .uuid) (ActionResult.withDefault [] model.assets)
                )
    in
    { model | groups = Groups.keepOnly existing model.groups }


{-| A rename, a move or a delete has landed: the lists are refetched rather
than patched, because a folder operation changes many paths at once.
-}
afterChange : UpdateConfig Msg msg -> AppState -> Model -> Result ApiError () -> ( Seed, Model, Cmd msg )
afterChange cfg appState model result =
    case result of
        Ok _ ->
            ( appState.seed
            , { model | dialog = NoDialog, dialogResult = Unset }
            , Cmd.map cfg.wrapMsg
                (Cmd.batch
                    [ DocumentTemplateDraftsApi.getFiles appState model.uuid GotFiles
                    , DocumentTemplateDraftsApi.getAssets appState model.uuid GotAssets
                    ]
                )
            )

        Err error ->
            ( appState.seed
            , { model | dialogResult = dialogError appState error }
            , RequestHelpers.getResultCmd cfg.logoutMsg result
            )


dialogError : AppState -> ApiError -> ActionResult String
dialogError appState error =
    ApiError.toActionResult appState (gettext "That did not work." appState.locale) error


{-| The path of whatever is selected — the thing itself, not the folder it is
in, which is what a rename, a move and a delete all act on.
-}
selectedName : Model -> String
selectedName model =
    Files.selectedPath (filesConfigOf model) model.selected


{-| What the open dialog does when it is confirmed.

A rename or a move of a **file** re-sends its content, and a file that has never
been opened is not in the cache — so its content is fetched first rather than
sent as an empty string, which is what the wizard does.

-}
confirmDialog : AppState -> Model -> Cmd Msg
confirmDialog appState model =
    let
        selected =
            selectedName model
    in
    case model.dialog of
        AddFileDialog ->
            DocumentTemplateDraftsApi.postFile appState
                model.uuid
                { uuid = Uuid.nil, fileName = into (Files.pathOf (filesConfigOf model) model.selected) model.dialogName }
                ""
                (AddedFile "")

        AddFolderDialog ->
            -- A folder with nothing in it exists only here: the server has no
            -- folder to make.
            Cmd.none

        RenameDialog ->
            renameTo appState model (into (Files.parentOf selected) model.dialogName)

        MoveDialog ->
            renameTo appState model (into (Maybe.withDefault "" model.moveTarget) (Files.nameOf selected))

        DeleteDialog ->
            case model.selected of
                Files.SelectedEntry (Files.File uuid) ->
                    DocumentTemplateDraftsApi.deleteFile appState model.uuid (Uuid.fromUuidString uuid) Deleted

                Files.SelectedEntry (Files.Asset uuid) ->
                    DocumentTemplateDraftsApi.deleteAsset appState model.uuid (Uuid.fromUuidString uuid) Deleted

                Files.SelectedFolder path ->
                    DocumentTemplateDraftsApi.deleteFolder appState model.uuid path (DeletedFolder path)

                Files.NoSelection ->
                    Cmd.none

        UploadDialog ->
            case FileUpload.file model.upload of
                Just file ->
                    let
                        path =
                            into (Files.pathOf (filesConfigOf model) model.selected) (File.name file)
                    in
                    if isEditable file then
                        -- Text becomes a file, so it can be edited here; a
                        -- file is sent with its content, so that is read first.
                        Task.perform (GotUploadContent path) (File.toString file)

                    else
                        DocumentTemplateDraftsApi.uploadAsset appState model.uuid path file UploadedAsset

                Nothing ->
                    Cmd.none

        NoDialog ->
            Cmd.none


{-| Whether an uploaded file is text the code editor can edit, as the wizard
decided it: by the type the browser gives it or, when it gives none — as it does
for `default.html.j2` — by the extension. A file the code editor has a language
for is text whatever the browser says about it.
-}
isEditable : File.File -> Bool
isEditable file =
    ContentType.isTextFile (File.name file) (File.mime file)
        || not (String.isEmpty (Files.language (File.name file)))


renameTo : AppState -> Model -> String -> Cmd Msg
renameTo appState model newPath =
    case model.selected of
        Files.SelectedEntry (Files.File uuid) ->
            case Dict.get uuid model.contents of
                Just (Success content) ->
                    DocumentTemplateDraftsApi.putFile appState
                        model.uuid
                        { uuid = Uuid.fromUuidString uuid, fileName = newPath }
                        content
                        RenamedFile

                _ ->
                    -- Its content has never been read, and a rename re-sends
                    -- it, so read it first (§6.1).
                    DocumentTemplateDraftsApi.getFileContent appState
                        model.uuid
                        (Uuid.fromUuidString uuid)
                        (GotFileContentForRename uuid newPath)

        Files.SelectedEntry (Files.Asset uuid) ->
            DocumentTemplateDraftsApi.putAsset appState
                model.uuid
                { uuid = Uuid.fromUuidString uuid, fileName = newPath, contentType = "", url = "", urlExpiration = appState.currentTime }
                RenamedFile

        -- A folder is moved by `moveFolderTo`, which also moves the names the
        -- client is holding for the folders inside it.
        Files.SelectedFolder _ ->
            Cmd.none

        Files.NoSelection ->
            Cmd.none


saveDraft : AppState -> Model -> Cmd Msg
saveDraft appState model =
    case model.draft of
        Success draft ->
            DocumentTemplateDraftsApi.putDraft appState
                model.uuid
                (Settings.encode "DraftDocumentTemplatePhase" draft)
                (SavedDraft model.settingsRevision)

        _ ->
            Cmd.none


{-| Every dirty file at once. Each answer clears that file; the settings save
waits for the last one (§3.1).
-}
saveDirtyFiles : AppState -> Model -> Cmd Msg
saveDirtyFiles appState model =
    let
        contentOf fileUuid =
            Dict.get fileUuid model.contents
                |> Maybe.andThen ActionResult.toMaybe
    in
    Cmd.batch
        (List.filterMap
            (\fileUuid ->
                Maybe.map
                    (\content ->
                        DocumentTemplateDraftsApi.putFileContent appState
                            model.uuid
                            (Uuid.fromUuidString fileUuid)
                            content
                            (SavedFile fileUuid content)
                    )
                    (contentOf fileUuid)
            )
            model.dirty
        )



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    Sub.batch
        [ Sub.map NavResizerMsg (Resizer.subscriptions model.navResizer)
        , Sub.map AddMenuMsg (Menu.subscriptions addMenuId model.addMenu)
        , Sub.map ProjectTypeHintMsg (TypeHint.subscriptions model.projectTypeHint)
        , Sub.map KMEditorTypeHintMsg (TypeHint.subscriptions model.kmEditorTypeHint)

        -- Cmd/Ctrl+Enter saves, as it does in every other form here. The
        -- draft is saved as a whole, so there is one thing for it to do
        -- wherever the caret happens to be.
        , if hasChanges model then
            CommonForm.submitShortcut appState { result = Unset, onSubmit = Save }

          else
            Sub.none
        ]



-- VIEW


view : AppState -> DTEditorRoute -> Model -> Html Msg
view appState route model =
    Page.actionResult appState (viewEditor appState route model) model.draft


viewEditor : AppState -> DTEditorRoute -> Model -> DocumentTemplateDraftDetail -> Html Msg
viewEditor appState route model draft =
    DetailPage.toHtml
        (DetailPage.detailPage
            { title = draft.name

            -- The tab first: a flush body gives its first child the height and
            -- sizes anything after it to what it holds, which for the dialogs
            -- is nothing.
            , content = [ viewTab appState route model, viewSaveErrorModal appState model, viewDiscardModal appState model, viewPublishModal appState model ]
            }
            |> DetailPage.withActions (actions appState model)
            |> DetailPage.withTabsWithTestIds
                { label = gettext "Document template editor" appState.locale
                , items =
                    List.map (tab appState model.uuid route)
                        [ ( Files, gettext "Files" appState.locale, ( Icon.regular "fa-file-code", "dt-editor_nav_files" ) )
                        , ( Preview, gettext "Preview" appState.locale, ( Icon.solid "fa-eye", "dt-editor_nav_preview" ) )
                        , ( Settings General, gettext "Settings" appState.locale, ( Icon.solid "fa-gear", "dt-editor_nav_settings" ) )
                        ]
                }
            |> DetailPage.withFlushContent True
        )


{-| Why the last save did not go through, in a dialog rather than on the page:
the draft is saved as a whole, so a failure belongs to the page rather than to
the tab that happened to be open, and the Files tab has no room for a band in
it. Each file that would not save is named — it stays unsaved, and a file left
dirty with no word about why reads as a file that saved. It opens once the save
has finished, not at the first failure while other files are still on their way.
-}
viewSaveErrorModal : AppState -> Model -> Html Msg
viewSaveErrorModal appState model =
    let
        fileErrors =
            model.savingFiles
                |> Dict.toList
                |> List.filterMap (\( fileUuid, answer ) -> Maybe.map (Tuple.pair (nameOfFile model fileUuid)) (failureOf answer))

        viewFileError ( fileName, message ) =
            Html.li [] [ strong [] [ text fileName ], text (": " ++ message) ]

        body =
            Maybe.unwrap [] (\message -> [ Html.p [] [ text message ] ]) model.saveError
                ++ (if List.isEmpty fileErrors then
                        []

                    else
                        [ Html.ul [] (List.map viewFileError fileErrors) ]
                   )
    in
    Modal.plain
        { title = gettext "Unable to save" appState.locale
        , open = not (isSaving model) && not (List.isEmpty body)
        , onClose = CloseSaveError
        , body = body
        }
        |> Modal.withCancelLabel (gettext "Close" appState.locale)
        |> Modal.withTestId "dt-editor_save-error-modal"
        |> Modal.toHtml


{-| With something unsaved: Save and Discard. Without: Publish.
-}
actions : AppState -> Model -> List (Button.Button Msg)
actions appState model =
    if hasChanges model then
        [ Button.primary (gettext "Save" appState.locale) Save
            |> Button.withIcon (Icon.solid "fa-floppy-disk")
            |> Button.withLoading (isSaving model)
            |> Button.withTestId "dt-editor_save"
        , Button.secondary (gettext "Discard" appState.locale) (SetDiscardOpen True)
            |> Button.withDisabled (isSaving model)
            |> Button.withTestId "dt-editor_discard"
        ]

    else if Feature.documentTemplateEditorsPublish appState then
        [ Button.primary (gettext "Publish" appState.locale) (SetPublishOpen True)
            |> Button.withIcon (Icon.solid "fa-cloud-arrow-up")
            |> Button.withTestId "dt-editor_publish"
        ]

    else
        []


{-| Discard throws away everything typed since the last save, so it asks first
— the wizard does not — and the button that does it says so in its color.
-}
viewDiscardModal : AppState -> Model -> Html Msg
viewDiscardModal appState model =
    ConfirmModal.dangerConfirm appState
        { open = model.discardOpen
        , title = gettext "Discard changes" appState.locale
        , body = [ text (gettext "Everything changed since the last save will be thrown away." appState.locale) ]
        , confirmLabel = gettext "Discard" appState.locale
        , onConfirm = Discard
        , onClose = SetDiscardOpen False
        , result = Unset
        , testId = Just "dt-editor_discard-modal"
        }


{-| What is about to be published, as it stands.

The same dialog the knowledge model editor publishes from, with what the wizard
put in this one: publishing is not undoable and the version it goes out under
cannot be used twice, so this is a last read of what is about to be fixed. The
metadata is not asked for again here — it belongs to the settings, and the
dialog says where to change it.

-}
viewPublishModal : AppState -> Model -> Html Msg
viewPublishModal appState model =
    case model.draft of
        Success draft ->
            let
                entry term description =
                    DescriptionList.entry { term = term, description = description }

                -- A real link rather than a button that navigates, so it
                -- survives middle-click and the screen reader's list of links.
                settingsLink =
                    Link.toHtml
                        (Link.link
                            { url = Routes.toUrl appState (Routes.documentTemplateEditorDetailSettings General model.uuid)
                            , label = gettext "Settings" appState.locale
                            }
                        )

                readme =
                    if String.isEmpty (String.trim draft.readme) then
                        []

                    else
                        [ Card.viewWithTitleLevel 4
                            (gettext "Readme" appState.locale)
                            [ Markdown.view { headingOffset = 4 } draft.readme ]
                        ]
            in
            Modal.confirm
                { title = gettext "Publish document template" appState.locale
                , open = model.publishOpen
                , onClose = SetPublishOpen False
                , body =
                    Alert.toHtmlWithContent
                        [ span []
                            (String.formatHtml
                                (gettext "Check the document template's metadata before publishing. You change them in %s." appState.locale)
                                [ settingsLink ]
                            )
                        ]
                        (Alert.info "")
                        :: Card.viewWithTitleLevel 4
                            (gettext "Metadata" appState.locale)
                            [ DescriptionList.toHtml
                                [ entry (gettext "Name" appState.locale) draft.name
                                , entry (gettext "Document Template ID" appState.locale) draft.id
                                , entry (gettext "Version" appState.locale) (Version.toString draft.version)
                                , entry (gettext "License" appState.locale) draft.license
                                , entry (gettext "Description" appState.locale) draft.description
                                ]
                            ]
                        :: readme
                , confirmLabel = gettext "Publish" appState.locale
                , onConfirm = Publish
                }
                |> Modal.withLoading (ActionResult.isLoading model.publishing)
                |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
                |> Modal.withHeaderAction (GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.documentTemplatesPublish)
                |> Modal.withWide
                |> Modal.withTestId "dt-editor_publish-modal"
                |> Modal.toHtml

        _ ->
            text ""


tab : AppState -> Uuid -> DTEditorRoute -> ( DTEditorRoute, String, ( Icon.Icon, String ) ) -> DetailPage.TabItem
tab appState uuid current ( route, label, ( icon, testId ) ) =
    { label = label
    , url = Routes.toUrl appState (routeOf uuid route)

    -- The settings are three sections behind one tab, so any of them is it.
    , active = sameTab current route
    , badge = Nothing
    , testId = Just testId
    , icon = Just icon
    }


sameTab : DTEditorRoute -> DTEditorRoute -> Bool
sameTab left right =
    case ( left, right ) of
        ( Settings _, Settings _ ) ->
            True

        _ ->
            left == right


routeOf : Uuid -> DTEditorRoute -> Routes.Route
routeOf uuid route =
    case route of
        Files ->
            Routes.documentTemplateEditorDetailFiles uuid

        Preview ->
            Routes.documentTemplateEditorDetailPreview uuid

        Settings section ->
            Routes.documentTemplateEditorDetailSettings section uuid


viewTab : AppState -> DTEditorRoute -> Model -> Html Msg
viewTab appState route model =
    case route of
        Files ->
            viewFilesTab appState model

        Settings section ->
            -- No wrapper of its own: the split navigation is the height of the
            -- tab and owns the scrolling of the section beside it.
            viewSettingsTab appState model section

        Preview ->
            -- No wrapper of its own: the document fills the tab, as the
            -- project's preview fills its own, and the toolbar over it is the
            -- only other thing in there.
            viewPreviewTab appState model


{-| The preview: which format, and the document that comes back. The project or
knowledge model editor it is rendered against is server state on the draft, so
it survives a reload.
-}
viewPreviewTab : AppState -> Model -> Html Msg
viewPreviewTab appState model =
    case model.draft of
        Success draft ->
            div [ class "c-dt-editor-preview" ]
                [ viewPreviewToolbar appState model draft
                , div [ class "c-dt-editor-preview__document", TestId.attribute "dt-editor_preview-document" ] [ viewPreviewDocument appState model draft ]
                ]

        _ ->
            text ""


{-| What the preview is made of, in one row: the kind of thing it is rendered
against, which one, a way to open it, the format, and — once there is something
on screen — a way to make it again.

The same row the wizard has, in the same order, because it is the order the
decisions are made in: what kind, which one, in what format.

-}
viewPreviewToolbar : AppState -> Model -> DocumentTemplateDraftDetail -> Html Msg
viewPreviewToolbar appState model draft =
    let
        mode testId label chosen onChoose =
            ModeSwitch.option { label = label, chosen = chosen, onChoose = onChoose }
                |> ModeSwitch.withTestId ("dt-editor_preview-mode_" ++ testId)

        ( source, sourceLink ) =
            case model.previewMode of
                ProjectPreview ->
                    ( TypeHint.viewCompact appState
                        { id = previewProjectId
                        , label = gettext "Project" appState.locale
                        , placeholder = gettext "Select project" appState.locale
                        , required = False
                        , error = Nothing
                        , hint = Nothing
                        , clearable = False
                        , viewItem = \project -> { label = project.name, code = Nothing, description = Nothing, icon = Nothing, badges = [] }
                        , toKey = Uuid.toString << .uuid
                        , wrapMsg = ProjectTypeHintMsg
                        }
                        model.projectTypeHint
                    , Maybe.map
                        (\project ->
                            ( Routes.toUrl appState (Routes.projectsDetailQuestionnaire project.uuid Nothing Nothing)
                            , gettext "Open the project" appState.locale
                            )
                        )
                        draft.project
                    )

                KMEditorPreview ->
                    ( TypeHint.viewCompact appState
                        { id = previewKMEditorId
                        , label = gettext "Knowledge model editor" appState.locale
                        , placeholder = gettext "Select knowledge model editor" appState.locale
                        , required = False
                        , error = Nothing
                        , hint = Nothing
                        , clearable = False
                        , viewItem = \kmEditor -> { label = kmEditor.name, code = Nothing, description = Nothing, icon = Nothing, badges = [] }
                        , toKey = Uuid.toString << .uuid
                        , wrapMsg = KMEditorTypeHintMsg
                        }
                        model.kmEditorTypeHint
                    , Maybe.map
                        (\kmEditor ->
                            ( Routes.toUrl appState (Routes.kmEditorEditor kmEditor.uuid Nothing)
                            , gettext "Open the knowledge model editor" appState.locale
                            )
                        )
                        draft.knowledgeModelEditor
                    )
    in
    div [ class "c-dt-editor-preview__toolbar" ]
        -- Two groups and an action: what the document is made from, in what
        -- format, and a way to make it again. What belongs together sits
        -- together, with more room between the groups than inside them.
        [ div [ class "c-dt-editor-preview__group" ]
            [ ModeSwitch.toHtmlOnSurface
                { label = gettext "What the preview is made from" appState.locale
                , options =
                    [ mode "project" (gettext "Project" appState.locale) (model.previewMode == ProjectPreview) (SetPreviewMode ProjectPreview)
                    , mode "km-editor" (gettext "KM editor" appState.locale) (model.previewMode == KMEditorPreview) (SetPreviewMode KMEditorPreview)
                    ]
                }
            , div [ class "c-dt-editor-preview__source" ] [ source ]
            , case sourceLink of
                Just ( url, label ) ->
                    Button.toHtml
                        (Button.iconOnlyLink (Icon.solid "fa-arrow-up-right-from-square") label url
                            |> Button.external
                            |> Button.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                            |> Button.small
                            |> Button.withTooltip
                            |> Button.withTestId "dt-editor_preview-open-source"
                        )

                Nothing ->
                    text ""
            ]
        , div [ class "c-dt-editor-preview__group c-dt-editor-preview__format" ]
            [ Field.toHtml
                (Field.select
                    { id = "dt-preview-format"
                    , label = gettext "Format" appState.locale
                    , value = Maybe.unwrap "" Uuid.toString draft.formatUuid
                    , options =
                        ( "", gettext "Choose a format" appState.locale )
                            :: List.map (\format -> ( Uuid.toString format.uuid, format.name )) draft.formats
                    , onInput = SetPreviewFormat
                    }
                )
            ]

        -- Nothing to make again until something has been made.
        , if DocumentTemplateDraftDetail.isPreviewSet draft then
            Button.toHtml
                (Button.iconOnly (Icon.solid "fa-rotate") (gettext "Reload preview" appState.locale) LoadPreview
                    |> Button.small
                    |> Button.withLoading (ActionResult.isLoading model.preview)
                    |> Button.withTooltipAt Tooltip.End
                    |> Button.withTestId "dt-editor_preview-reload"
                )

          else
            text ""
        ]


viewPreviewDocument : AppState -> Model -> DocumentTemplateDraftDetail -> Html Msg
viewPreviewDocument appState model draft =
    if not (DocumentTemplateDraftDetail.isPreviewSet draft) then
        Message.message
            { title = gettext "Preview not set" appState.locale
            , description = gettext "Choose what to render the template against, and in which format." appState.locale
            }
            |> Message.withIllustration (Illustration.toHtml Illustration.placeholder)
            |> Message.withinPage
            |> Message.toHtml

    else
        case ( model.preview, model.previewUnsupported ) of
            ( Success urlResponse, _ ) ->
                iframe
                    [ src urlResponse.url
                    , class "c-dt-editor-preview__frame"
                    , TestId.attribute "dt-editor_preview-frame"
                    , title (gettext "Document preview" appState.locale)
                    ]
                    []

            ( Error error, _ ) ->
                DocumentPreviewError.view appState { message = error, log = model.previewLog }

            ( _, Just documentUrl ) ->
                Message.message
                    { title = gettext "Download preview" appState.locale
                    , description = gettext "The document format cannot be displayed in the web browser. You can still download and view it." appState.locale
                    }
                    |> Message.withIllustration (Illustration.toHtml Illustration.download)
                    |> Message.withAction
                        (Button.linkPrimary (gettext "Download" appState.locale) documentUrl
                            |> Button.withIcon (Icon.solid "fa-download")
                            |> Button.external
                            |> Button.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                            |> Button.withTestId "dt-editor_preview-download"
                        )
                    |> Message.withinPage
                    |> Message.toHtml

            _ ->
                Page.loading appState


{-| The settings as the application's settings are: a menu of sections, and the
one being edited beside it. Three sections rather than one long page, because
the formats alone run longer than a screen and the general fields are what
somebody opening the settings came for.
-}
viewSettingsTab : AppState -> Model -> DTSettingsSection -> Html Msg
viewSettingsTab appState model section =
    case model.draft of
        Success draft ->
            let
                item ( to, label ) =
                    { label = label
                    , url = Routes.toUrl appState (Routes.documentTemplateEditorDetailSettings to model.uuid)
                    , active = to == section
                    , testId = Just ("dt-editor_settings-nav_" ++ sectionTestId to)
                    }
            in
            SplitNav.toHtmlWithTestIds
                { title = gettext "Settings" appState.locale
                , groups =
                    [ ( Nothing
                      , List.map item
                            [ ( General, gettext "General" appState.locale )
                            , ( KnowledgeModels, gettext "Knowledge Models" appState.locale )
                            , ( Formats, gettext "Formats" appState.locale )
                            ]
                      )
                    ]
                , body = [ viewSettingsSection appState model draft section ]
                , footer = Nothing
                }

        _ ->
            text ""


sectionTestId : DTSettingsSection -> String
sectionTestId section =
    case section of
        General ->
            "general"

        KnowledgeModels ->
            "knowledge-models"

        Formats ->
            "formats"


viewSettingsSection : AppState -> Model -> DocumentTemplateDraftDetail -> DTSettingsSection -> Html Msg
viewSettingsSection appState model draft section =
    let
        ( title, body ) =
            case section of
                General ->
                    ( gettext "General" appState.locale, Settings.general config )

                KnowledgeModels ->
                    ( gettext "Knowledge Models" appState.locale, Settings.knowledgeModels config )

                Formats ->
                    ( gettext "Formats" appState.locale, Settings.formats config )

        config =
            { locale = appState.locale
            , draft = draft
            , onChange = SetDraft
            , formatPrefabs = ActionResult.withDefault [] model.formatPrefabs
            , onAddFormat = AddFormat
            }
    in
    Content.content { title = title, body = [ body ] }
        |> Content.form
        |> Content.withinPage
        -- The same page of the guide from all three sections: it is one page
        -- about these settings, not one per section.
        |> Content.withActions [ GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.documentTemplatesSettings ]
        |> Content.toHtml


viewFilesTab : AppState -> Model -> Html Msg
viewFilesTab appState model =
    let
        config =
            filesConfig appState model
    in
    Workspace.toHtml
        (Workspace.workspace
            { id = workspaceId
            , main = [ Files.view config (viewOpen appState model), viewDialogs appState model ]
            }
            -- The editor is the content area, not something set into it: the
            -- text starts where the pane starts.
            |> Workspace.withFlushMain
            |> Workspace.withFlushNavHeader
            |> Workspace.withNav
                { label = gettext "Files" appState.locale
                , header = [ fileActions appState model ]
                , content = [ Files.tree config ]
                , resizer = model.navResizer
                , toResizerMsg = NavResizerMsg
                , resizerWords = UiWords.resizer appState (gettext "Resize the file list" appState.locale)
                , overlayOpen = model.navOverlayOpen
                , onOverlay = SetNavOverlay
                , toggleLabel = gettext "Files" appState.locale
                , closeLabel = gettext "Close the file list" appState.locale
                }
        )


{-| An asset is not text: an image is shown, a PDF is shown where the browser
can, and anything else is a way to download it.
-}
viewAsset : AppState -> Model -> String -> Html Msg
viewAsset appState model assetUuid =
    case Maybe.withDefault Unset (Dict.get assetUuid model.assetCache) of
        Success asset ->
            if ContentType.isImage asset.contentType then
                div [ class "c-dt-editor-files__asset" ] [ img [ src asset.url, alt asset.fileName ] [] ]

            else if asset.contentType == "application/pdf" && appState.navigator.pdf then
                div [ class "c-dt-editor-files__asset" ] [ iframe [ src asset.url, title asset.fileName ] [] ]

            else
                div [ class "c-dt-editor-files__empty" ]
                    [ Message.message
                        { title = gettext "Download file" appState.locale
                        , description = gettext "This file cannot be shown here. You can still download it." appState.locale
                        }
                        |> Message.withIllustration (Illustration.toHtml Illustration.download)
                        |> Message.withAction
                            (Button.linkPrimary (gettext "Download" appState.locale) asset.url
                                |> Button.withIcon (Icon.solid "fa-download")
                                |> Button.external
                                |> Button.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                                |> Button.withTestId "dt-editor_asset-download"
                            )
                        |> Message.withinPage
                        |> Message.toHtml
                    ]

        Error error ->
            div [ class "c-dt-editor-files__empty" ] [ Page.errorWithinPage appState error ]

        _ ->
            div [ class "c-dt-editor-files__empty" ] [ text (gettext "Loading..." appState.locale) ]


{-| Every folder on the way to `path`, including it: what has to be open for
something to be seen where it is.
-}
ancestorsOf : String -> Set String
ancestorsOf path =
    String.split "/" path
        |> List.indexedMap (\index _ -> String.join "/" (List.take (index + 1) (String.split "/" path)))
        |> List.filterNot String.isEmpty
        |> Set.fromList


{-| The ids of the two searches, which are also what their labels point at.
-}
previewProjectId : String
previewProjectId =
    "dt-preview-project"


previewKMEditorId : String
previewKMEditorId =
    "dt-preview-km-editor"


{-| What the preview is rendered against, taken from the draft: the search
fields show what was chosen last time, and the switch opens on the kind of
thing it is.
-}
withPreviewSource : ActionResult DocumentTemplateDraftDetail -> Model -> Model
withPreviewSource result model =
    case result of
        Success draft ->
            { model
                | projectTypeHint = TypeHint.setSelected draft.project model.projectTypeHint
                , kmEditorTypeHint = TypeHint.setSelected draft.knowledgeModelEditor model.kmEditorTypeHint
                , previewMode =
                    if Maybe.isJust draft.knowledgeModelEditorUuid then
                        KMEditorPreview

                    else
                        ProjectPreview
            }

        _ ->
            model


{-| Changing what the preview is made of: the draft is updated here, sent, and
the preview asked for again once the server has the settings and they are
complete — it is a document made of the settings, so a settings change makes
the one on screen wrong.
-}
savePreviewSettings :
    AppState
    -> Model
    -> (DocumentTemplateDraftPreviewSettings -> DocumentTemplateDraftPreviewSettings)
    -> ( Model, Cmd Msg )
savePreviewSettings appState model change =
    case model.draft of
        Success draft ->
            let
                settings =
                    change (DocumentTemplateDraftDetail.getPreviewSettings draft)
            in
            ( { model
                | draft = Success (DocumentTemplateDraftDetail.updatePreviewSettings settings draft)
                , preview = Unset
                , previewUnsupported = Nothing
              }
            , DocumentTemplateDraftsApi.putPreviewSettings appState model.uuid settings SavedPreviewSettings
            )

        _ ->
            ( model, Cmd.none )


{-| A path inside a folder, or at the top of the template when there is no
folder.
-}
into : String -> String -> String
into folder name =
    if String.isEmpty folder then
        name

    else
        folder ++ "/" ++ name


{-| Whether this folder is one the client is holding rather than one the server
has, in which case renaming, moving or deleting it is a change to `newFolders`
and nothing else (`Files.hasContent`).
-}
isClientFolder : Model -> String -> Bool
isClientFolder model path =
    not (Files.hasContent (filesConfigOf model) path)


{-| Renaming or moving a folder: the server moves what is in it, and the client
moves the names it is holding — including the folder itself, which would
otherwise stay behind as an empty folder beside the new one.
-}
moveFolderTo : AppState -> Model -> String -> String -> ( Model, Cmd Msg )
moveFolderTo appState model from to =
    if isClientFolder model from then
        ( { model
            | newFolders = List.map (Files.renamePath from to) model.newFolders
            , selected = Files.SelectedFolder to
            , dialog = NoDialog
            , dialogName = ""
            , dialogResult = Unset
            , moveTarget = Nothing
          }
        , Cmd.none
        )

    else
        ( { model | dialogResult = Loading }
        , DocumentTemplateDraftsApi.moveFolder appState model.uuid from to (MovedFolder from to)
        )


{-| The id of a file's editor, which is the id the code editor puts on the text
itself — so it is what focus is moved to.
-}
fileEditorId : String -> String
fileEditorId fileUuid =
    "dt-editor-file-" ++ fileUuid


workspaceId : String
workspaceId =
    "dt-editor"


{-| Where focus goes when an entry is opened from the narrow file list: a file's
editor, which the port waits for, or the work itself for an asset, which has
nothing to type into.
-}
openedId : Files.Entry -> String
openedId entry =
    case entry of
        Files.File fileUuid ->
            fileEditorId fileUuid

        Files.Asset _ ->
            Workspace.mainId workspaceId


{-| What the template is called, which is what the top of its tree is called.
-}
draftName : Model -> String
draftName model =
    Maybe.unwrap "" .name (ActionResult.toMaybe model.draft)


{-| The path of whatever a tab is on, whichever of the two lists it is in.
-}
entryName : Model -> Files.Entry -> String
entryName model entry =
    case entry of
        Files.File fileUuid ->
            nameOfFile model fileUuid

        Files.Asset assetUuid ->
            ActionResult.withDefault [] model.assets
                |> List.find (\asset -> Uuid.toString asset.uuid == assetUuid)
                |> Maybe.map .fileName
                |> Maybe.withDefault ""


nameOfFile : Model -> String -> String
nameOfFile model fileUuid =
    ActionResult.withDefault [] model.files
        |> List.find (\file -> Uuid.toString file.uuid == fileUuid)
        |> Maybe.map .fileName
        |> Maybe.withDefault ""


{-| What can be done to whatever is selected. The root has only Add; a folder
can be renamed, moved and deleted; a file or an asset the same.
-}
fileActions : AppState -> Model -> Html Msg
fileActions appState model =
    let
        -- The test id is given rather than made from the label: a label is
        -- translated, and an id that changes with the language is an id nothing
        -- can be written against.
        action toButton testId label icon dialog =
            Button.toHtml
                (toButton (Icon.solid icon) label (OpenDialog dialog)
                    |> Button.small
                    |> Button.withTooltip
                    |> Button.withTestId ("dt-editor_file-tree_" ++ testId)
                )

        menuItem testId label icon dialog =
            Menu.item label (OpenDialog dialog)
                |> Menu.withIcon (Icon.solid icon)
                |> Menu.withTestId ("dt-editor_file-tree_" ++ testId)

        forSelection =
            case model.selected of
                Files.NoSelection ->
                    []

                _ ->
                    [ action Button.iconOnly "rename" (gettext "Rename" appState.locale) "fa-pen-to-square" RenameDialog
                    , action Button.iconOnly "move" (gettext "Move" appState.locale) "fa-file-import" MoveDialog
                    , action Button.dangerIconOnly "delete" (gettext "Delete" appState.locale) "fa-trash" DeleteDialog
                    ]
    in
    div [ class "c-dt-editor-files__actions" ]
        [ -- Adding is about the template, so it is always there, on the side
          -- the reading starts from. What is done to one file is not, and
          -- appears at the other end when there is a file to do it to.
          Menu.toHtml
            { id = addMenuId

            -- The same trigger the project toolbar's menus use, because a menu
            -- in a toolbar is the same thing wherever the toolbar is.
            , trigger = Menu.quiet (gettext "Add" appState.locale)
            , alignment = Menu.Start
            , items =
                [ menuItem "add-folder" (gettext "Folder" appState.locale) "fa-folder-plus" AddFolderDialog
                , menuItem "add-file" (gettext "File" appState.locale) "fa-file-circle-plus" AddFileDialog
                , Menu.separator
                , menuItem "upload" (gettext "Upload" appState.locale) "fa-upload" UploadDialog
                ]
            , toMsg = AddMenuMsg
            }
            model.addMenu
        , div [ class "c-dt-editor-files__actions-end" ] forSelection
        ]


{-| The Add menu, which is one menu on one page, so one id.
-}
addMenuId : String
addMenuId =
    "dt-editor_file-tree_add"


{-| How many of the menu's entries the arrow keys walk: the separator is not
one of them.
-}
addMenuItems : Int
addMenuItems =
    3


{-| One dialog element, whichever is open: a name to type, a folder to move
into, a file to upload, or a delete to confirm.
-}
viewDialogs : AppState -> Model -> Html Msg
viewDialogs appState model =
    case model.dialog of
        NoDialog ->
            text ""

        DeleteDialog ->
            ConfirmModal.dangerConfirm appState
                { open = True
                , title = gettext "Delete" appState.locale
                , body =
                    -- Named, and named in the question: a dialog that says
                    -- "this will be deleted" asks somebody to remember what
                    -- they clicked.
                    [ Text.bodyHtml
                        (String.formatHtml
                            (case model.selected of
                                Files.SelectedFolder _ ->
                                    gettext "Are you sure you want to permanently delete %s and everything in it?" appState.locale

                                _ ->
                                    gettext "Are you sure you want to permanently delete %s?" appState.locale
                            )
                            [ strong [] [ text (Files.nameOf (selectedName model)) ] ]
                        )
                    ]
                , confirmLabel = gettext "Delete" appState.locale
                , onConfirm = ConfirmDialog
                , onClose = CloseDialog
                , result = model.dialogResult
                , testId = Just "dt-editor_delete-modal"
                }

        UploadDialog ->
            dialogShell appState
                model
                (gettext "Upload a file" appState.locale)
                (gettext "Upload" appState.locale)
                [ FileUpload.toHtml
                    { toMsg = UploadMsg
                    , label = gettext "File" appState.locale
                    , buttonLabel = gettext "Choose a file" appState.locale
                    , hint = gettext "A text file is added as a file you can edit here. Anything else is stored as an asset." appState.locale
                    , invalid = False
                    , onClear = Nothing
                    }
                    model.upload
                ]

        MoveDialog ->
            -- The same dialog the knowledge model editor moves an entity with:
            -- the tree it already lives in, with what it may not go into left
            -- in place and out of reach.
            Modal.plain
                { title = gettext "Move" appState.locale
                , open = True
                , onClose = CloseDialog
                , body =
                    [ div [ class "c-dt-editor-move" ]
                        [ div [ class "c-dt-editor-move__tree" ]
                            [ Files.moveTree (filesConfigOf model)
                                { selection = model.selected
                                , target = model.moveTarget
                                , expanded = model.moveExpanded
                                , onSelect = MoveSelect
                                , onToggle = MoveToggle
                                }
                            ]
                        , case failureOf model.dialogResult of
                            Just message ->
                                Alert.toHtml (Alert.danger message)

                            Nothing ->
                                text ""
                        ]
                    ]
                }
                |> Modal.withActions
                    [ Button.ghost (gettext "Cancel" appState.locale) CloseDialog
                    , Button.primary (gettext "Move" appState.locale) ConfirmDialog
                        |> Button.withDisabled (Maybe.isNothing model.moveTarget)
                        |> Button.withLoading (ActionResult.isLoading model.dialogResult)
                        |> Button.withTestId "dt-editor_move-modal_move"
                    ]
                |> Modal.withTestId "dt-editor_move-modal"
                |> Modal.toHtml

        _ ->
            dialogShell appState
                model
                (dialogTitle appState model.dialog)
                (dialogConfirm appState model.dialog)
                [ Field.toHtml
                    (Field.input
                        { id = "dt-editor-dialog-name"
                        , label = gettext "Name" appState.locale
                        , value = model.dialogName
                        , onInput = SetDialogName
                        }
                    )
                ]


dialogShell : AppState -> Model -> String -> String -> List (Html Msg) -> Html Msg
dialogShell appState model title confirmLabel body =
    Modal.plain
        { title = title
        , open = True
        , onClose = CloseDialog

        -- Why it did not work, in the dialog that asked. Without this a failed
        -- rename or upload closes nothing and says nothing.
        , body =
            body
                ++ (case failureOf model.dialogResult of
                        Just message ->
                            [ Alert.toHtml (Alert.danger message) ]

                        Nothing ->
                            []
                   )
        }
        -- A real form, so Enter does what the button does and the footer brings
        -- its own way out: `withActions` replaces the footer, and a dialog whose
        -- only way out is the Escape key is a dialog some people cannot leave.
        |> Modal.withForm
            { onSubmit = ConfirmDialog
            , submitLabel = confirmLabel
            }
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> Modal.withLoading (ActionResult.isLoading model.dialogResult)
        |> Modal.withTestId "dt-editor_dialog"
        |> Modal.toHtml


dialogTitle : AppState -> Dialog -> String
dialogTitle appState dialog =
    case dialog of
        AddFileDialog ->
            gettext "New file" appState.locale

        AddFolderDialog ->
            gettext "New folder" appState.locale

        RenameDialog ->
            gettext "Rename" appState.locale

        _ ->
            ""


dialogConfirm : AppState -> Dialog -> String
dialogConfirm appState dialog =
    case dialog of
        RenameDialog ->
            gettext "Rename" appState.locale

        _ ->
            gettext "Create" appState.locale


filesConfig : AppState -> Model -> Files.Config Msg
filesConfig appState model =
    let
        config =
            filesConfigOf model
    in
    { config | locale = appState.locale }


{-| The parts of the config that do not need the locale, for the path helpers
the update uses.
-}
filesConfigOf : Model -> Files.Config Msg
filesConfigOf model =
    { locale = Gettext.defaultLocale
    , name = draftName model
    , files = ActionResult.withDefault [] model.files
    , assets = ActionResult.withDefault [] model.assets
    , dirty = model.dirty
    , selected = model.selected
    , collapsed = model.collapsed
    , newFolders = model.newFolders
    , onOpen = Open
    , onSelectRoot = Select Files.NoSelection
    , onSelectFolder = Select << Files.SelectedFolder
    , onToggleFolder = ToggleFolder
    }


{-| What is open, on one side or on two: a strip of tabs over the file being
worked on.
-}
viewOpen : AppState -> Model -> List (Html Msg)
viewOpen appState model =
    if List.isEmpty (Groups.openEntries model.groups) then
        []

    else if Groups.isSplit model.groups then
        [ viewSide appState model Groups.First, viewSide appState model Groups.Second ]

    else
        [ viewSide appState model Groups.First ]


viewSide : AppState -> Model -> Groups.Side -> Html Msg
viewSide appState model side =
    div
        [ classList
            [ ( "c-dt-editor-files__side", True )

            -- Which side the next file opens into, and which one the toolbar's
            -- actions act on. Only worth saying when there are two.
            , ( "is-active", Groups.isSplit model.groups && Groups.activeSide model.groups == side )
            ]
        ]
        [ viewTabs appState model side
        , case Groups.currentOf side model.groups of
            Just entry ->
                viewEntry appState model entry

            Nothing ->
                Files.nothingOpen appState.locale
        ]


{-| The files open on this side, in the order they were opened.

Not a `role="tablist"`: a tab there is a way of showing one panel of several
that belong together, and these are open documents, each of which can be closed.
So they are buttons in a named group, and the one being edited says so with
`aria-current`, which is what it is — the current one of a set of like things.

-}
viewTabs : AppState -> Model -> Groups.Side -> Html Msg
viewTabs appState model side =
    div [ class "c-dt-editor-files__strip" ]
        [ div
            [ class "c-dt-editor-files__tabs"
            , attribute "role" "group"
            , attribute "aria-label"
                (if Groups.isSplit model.groups then
                    case side of
                        Groups.First ->
                            gettext "Open files, first column" appState.locale

                        Groups.Second ->
                            gettext "Open files, second column" appState.locale

                 else
                    gettext "Open files" appState.locale
                )
            ]
            (List.map (viewOpenFileTab appState model side) (Groups.tabsOf side model.groups))

        -- Beside the tabs rather than after them: what is done to the editor
        -- stays where it is while the tabs scroll past, and its tooltip is not
        -- cut off by the edge of a scrolling box.
        , viewTabActions appState model side
        ]


viewOpenFileTab : AppState -> Model -> Groups.Side -> Files.Entry -> Html Msg
viewOpenFileTab appState model side entry =
    let
        fileName =
            entryName model entry

        name =
            Files.nameOf fileName

        current =
            Groups.currentOf side model.groups == Just entry

        unsaved =
            case entry of
                Files.File fileUuid ->
                    List.member fileUuid model.dirty

                Files.Asset _ ->
                    False
    in
    div
        [ classList
            [ ( "c-dt-editor-files__tab", True )
            , ( "is-current", current )
            ]
        ]
        [ button
            ([ class "c-dt-editor-files__tab-name"
             , type_ "button"
             , onClick (Activate side entry)
             , TestId.attribute "dt-editor_tab"
             ]
                ++ (if current then
                        [ attribute "aria-current" "true" ]

                    else
                        []
                   )
            )
            [ Icon.toHtml (Files.iconOf (filesConfigOf model) entry fileName)
            , text name

            -- A dot is a color, and a color is never the only signal (1.4.1):
            -- the word is there for anything that does not see it.
            , if unsaved then
                span [ class "c-dt-editor-files__tab-unsaved" ]
                    [ Text.visuallyHiddenInline (gettext "Unsaved" appState.locale) ]

              else
                text ""
            ]
        , Button.toHtml
            (Button.iconOnly (Icon.solid "fa-xmark")
                (gettext "Close " appState.locale ++ name)
                (CloseTab side entry)
                |> Button.small
                |> Button.quiet
                |> Button.withTooltip
                |> Button.withTestId "dt-editor_tab-close"
            )
        ]


{-| What can be done to the editor rather than to the file: how long lines are
shown, and which side the file is on.
-}
viewTabActions : AppState -> Model -> Groups.Side -> Html Msg
viewTabActions appState model side =
    let
        wrapLabel =
            if model.wrap then
                gettext "Stop wrapping long lines" appState.locale

            else
                gettext "Wrap long lines" appState.locale
    in
    div [ class "c-dt-editor-files__tab-actions" ]
        [ Button.toHtml
            (Button.iconOnly (Icon.solid "fa-turn-down") wrapLabel ToggleWrap
                |> Button.small
                |> Button.quiet
                |> Button.withPressed model.wrap
                -- At the end of the strip, so a bubble centered under it would
                -- hang off the side of the screen.
                |> Button.withTooltipAt Tooltip.End
                |> Button.withTestId "dt-editor_wrap"
            )
        , Button.toHtml
            (Button.iconOnly
                (Icon.solid
                    (if Groups.isSplit model.groups then
                        -- With both sides open the action is a swap between
                        -- them; with one, it is the second side being made.
                        "fa-arrow-right-arrow-left"

                     else
                        "fa-table-columns"
                    )
                )
                (gettext "Move to the other side" appState.locale)
                (MoveToOtherSide side)
                |> Button.small
                |> Button.quiet
                |> Button.withTooltipAt Tooltip.End
                |> Button.withDisabled (Maybe.isNothing (Groups.currentOf side model.groups))
                |> Button.withTestId "dt-editor_move-to-other-side"
            )
        ]


{-| A file in the code editor, or an asset, which is looked at rather than
edited.
-}
viewEntry : AppState -> Model -> Files.Entry -> Html Msg
viewEntry appState model entry =
    case entry of
        Files.File fileUuid ->
            case Dict.get fileUuid model.contents of
                Just (Success content) ->
                    let
                        fileName =
                            nameOfFile model fileUuid
                    in
                    Field.toHtml
                        (Field.code
                            { id = fileEditorId fileUuid
                            , label = Files.nameOf fileName
                            , language = Files.language fileName
                            , value = content
                            , onChange = FileChanged fileUuid
                            }
                            |> Field.withHiddenLabel
                            |> Field.withCodeWrap model.wrap
                        )

                Just (Error error) ->
                    div [ class "c-dt-editor-files__empty" ] [ Page.errorWithinPage appState error ]

                _ ->
                    div [ class "c-dt-editor-files__empty" ] [ text (gettext "Loading..." appState.locale) ]

        Files.Asset assetUuid ->
            viewAsset appState model assetUuid


{-| What the page is about, by name, for the window's title, once it has
arrived.
-}
entityName : Model -> Maybe String
entityName model =
    Maybe.map .name (ActionResult.toMaybe model.draft)
