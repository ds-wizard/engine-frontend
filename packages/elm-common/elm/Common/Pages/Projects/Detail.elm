module Common.Pages.Projects.Detail exposing
    ( Model
    , Msg
    , SaveError
    , entityName
    , init
    , initialModel
    , isGuarded
    , reenter
    , subscriptions
    , update
    , view
    )

{-| One project, with the questionnaire, its metrics, its preview, its
documents, its files, any plugin tabs and its settings as tabs.

This is the frame: the header, the tabs, what each tab loads, and the
collaboration websocket. The bodies of the tabs are being rebuilt one at a
time; a tab that has not had its turn yet is a placeholder inside the frame, so
the project's name, its tabs and the people online are all there and only the
content is missing. Whole so far: the plugin tabs, because a plugin tab is one
custom element, the documents tab with the form that adds to it, the files tab,
the settings tab, the preview and the metrics.

**The header's actions belong to whoever is looking.** An owner gets the Share
button and a kebab holding the link to the project and the sentence saying who
can already reach it; somebody signed in looking at a project shared with
everybody gets "Add to my projects", which makes them its owner. Everyone else
gets neither, as in the wizard.

The tabs are routes, and project detail routes are not
`Common.Routes.isSameListingRoute`, so moving between them re-runs `init` — which
is where the wizard fetched the data for the subroute it was going to, and
where this does too. The feature router keeps the page model while the project
is the same, so the header does not blink between tabs.

**A tab's own model is built by a message, not by `init`.** The router keeps
this page's model while the project is the same and throws away the one `init`
returns, so a tab that has state of its own — the documents listing's query
string, the new document form — is set up by a message `init` sends, which the
model that is kept receives.

**The websocket is opened once the project has loaded and stays open while the
page does.** It is what makes the project collaborative: the server sends who
else is here and what they changed. A change to the questionnaire (`SetContent`,
`AddFile`) is applied to it (`Questionnaire.applyProjectEvent`, `addFile`), and
the user's own changes are sent from it; the rest — the user list, a change to
the project itself, being disconnected — are handled here.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError(..))
import Common.Api.Models.Member as Member
import Common.Api.Models.OnlineUserInfo exposing (OnlineUserInfo)
import Common.Api.Models.ProjectCommon as ProjectCommon exposing (ProjectCommon)
import Common.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Common.Api.Models.ProjectDetailWrapper exposing (ProjectDetailWrapper)
import Common.Api.Models.ProjectPerm as ProjectPerm
import Common.Api.Models.ProjectPreview exposing (ProjectPreview)
import Common.Api.Models.ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Common.Api.Models.ProjectSettings exposing (ProjectSettings)
import Common.Api.Models.SummaryReport exposing (SummaryReport)
import Common.Api.Models.WebSockets.ClientProjectMessage as ClientProjectMessage
import Common.Api.Models.WebSockets.ServerProjectMessage as ServerProjectMessage
import Common.Api.Models.WebSockets.WebSocketServerAction as WebSocketServerAction
import Common.Api.Projects as ProjectsApi
import Common.Api.ServerError as ServerError
import Common.Api.WebSocket as WebSocket exposing (WebSocket)
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.DetailPage as DetailPage
import Common.Components.ItemActions as ItemActions
import Common.Components.OnlineUsers as OnlineUsers
import Common.Components.Page as Page
import Common.Components.PluginView as PluginView
import Common.Components.Questionnaire as Questionnaire
import Common.Components.SavingIndicator as SavingIndicator
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.Forms.ProjectShareForm as ProjectShareForm
import Common.Data.NotFound as NotFound
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Data.ProjectUtils as ProjectUtils
import Common.Data.Session as Session
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.Projects.Components.ProjectSharing as ProjectSharing
import Common.Pages.Projects.Components.RevertModal as RevertModal
import Common.Pages.Projects.Components.ShareModal as ShareModal
import Common.Pages.Projects.Components.VersionViewModal as VersionViewModal
import Common.Pages.Projects.Detail.Documents as Documents
import Common.Pages.Projects.Detail.Files as Files
import Common.Pages.Projects.Detail.Metrics as Metrics
import Common.Pages.Projects.Detail.NewDocument as NewDocument
import Common.Pages.Projects.Detail.Preview as Preview
import Common.Pages.Projects.Detail.Routes as ProjectDetailRoute exposing (ProjectDetailRoute)
import Common.Pages.Projects.Detail.Settings as Settings
import Common.Pages.Projects.Routes as ProjectsRoutes
import Common.Plugins.Plugin as Plugin exposing (Plugin, ProjectTabConnector)
import Common.Plugins.PluginElement as PluginElement
import Common.Ports.Copy as Copy
import Common.Ports.Window as Window
import Common.Routes as Routes
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Common.Utils.RequestHelpers as RequestHelpers
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Form
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Decode as D
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Seed)
import String.Format as String
import Task.Extra as Task
import Ui.Alert as Alert
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.EditableTitle as EditableTitle
import Ui.Icon as Icon
import Ui.Illustration as Illustration
import Ui.Layout as Layout
import Ui.Menu as Menu
import Ui.Modal as Modal
import Ui.Page.Message as Message
import Ui.Text as Text
import Uuid exposing (Uuid)



-- MODEL


type alias Model =
    { uuid : Uuid
    , common : ActionResult ProjectCommon
    , questionnaire : ActionResult ProjectQuestionnaire
    , questionnaireModel : ActionResult Questionnaire.Model
    , questionnaireLink : { path : Maybe String, commentThreadUuid : Maybe Uuid }
    , savingEventUuids : List Uuid
    , saving : SavingIndicator.Model
    , replyDebounces : Dict String (Debounce ProjectEvent)
    , preview : ActionResult ProjectPreview
    , summaryReport : ActionResult SummaryReport
    , onlineUsers : List OnlineUserInfo
    , websocket : WebSocket
    , offline : Bool
    , saveError : Maybe SaveError
    , forceDisconnect : Bool
    , actionsMenu : Menu.Model
    , shareModal : ShareModal.Model
    , versionViewModal : VersionViewModal.Model
    , revertModal : RevertModal.Model
    , addingToMyProjects : ActionResult ()

    -- The title, renamed where it stands, and while the new name is on its way
    -- the name it had, to put back if the server refuses it.
    , title : EditableTitle.Model
    , renamingFrom : Maybe String
    , copiedLink : Bool
    , previewModel : Preview.Model
    , documents : Documents.Model
    , newDocument : NewDocument.Model
    , files : Files.Model
    , settings : Settings.Model
    }


initialModel : AppState -> Uuid -> Model
initialModel appState uuid =
    { uuid = uuid
    , common = Loading
    , questionnaire = Loading
    , questionnaireModel = Loading
    , questionnaireLink = { path = Nothing, commentThreadUuid = Nothing }
    , savingEventUuids = []
    , saving = SavingIndicator.init
    , replyDebounces = Dict.empty
    , preview = Loading
    , summaryReport = Loading
    , onlineUsers = []
    , websocket = ProjectsApi.websocket uuid
    , offline = False
    , saveError = Nothing
    , forceDisconnect = False
    , actionsMenu = Menu.init
    , shareModal = ShareModal.initialModel
    , versionViewModal = VersionViewModal.initEmpty
    , revertModal = RevertModal.init
    , addingToMyProjects = Unset
    , title = EditableTitle.init
    , renamingFrom = Nothing
    , copiedLink = False
    , previewModel = Preview.initialModel
    , documents = Documents.initialModel
    , newDocument = NewDocument.initialModel Nothing
    , files = Files.initialModel
    , settings = Settings.initialModel appState
    }


{-| Each tab loads what it needs, and every one of those endpoints answers with
the project's common data beside the tab's own, so whichever tab is entered
first fills the header. A tab with state of its own is set up beside that
request, as the wizard batched `Documents.fetchData` with the common one.
-}
init : AppState -> Uuid -> ProjectDetailRoute -> ( Model, Cmd Msg )
init appState uuid route =
    ( let
        model =
            initialModel appState uuid
      in
      case route of
        ProjectDetailRoute.Questionnaire mbPath mbCommentThreadUuid ->
            { model | questionnaireLink = { path = mbPath, commentThreadUuid = mbCommentThreadUuid } }

        _ ->
            model
    , case route of
        ProjectDetailRoute.Questionnaire _ _ ->
            ProjectsApi.getQuestionnaire appState uuid GetQuestionnaireCompleted

        ProjectDetailRoute.Preview ->
            ProjectsApi.getPreview appState uuid GetPreviewCompleted

        ProjectDetailRoute.Metrics ->
            ProjectsApi.getSummaryReport appState uuid GetSummaryReportCompleted

        ProjectDetailRoute.Documents pqs ->
            Cmd.batch
                [ ProjectsApi.get appState uuid GetCommonCompleted
                , Task.dispatch (InitDocuments pqs)
                ]

        ProjectDetailRoute.NewDocument mbEventUuid ->
            Cmd.batch
                [ ProjectsApi.getSettings appState uuid GetSettingsCompleted
                , Task.dispatch (InitNewDocument mbEventUuid)
                ]

        ProjectDetailRoute.Files pqs ->
            Cmd.batch
                [ ProjectsApi.get appState uuid GetCommonCompleted
                , Task.dispatch (InitFiles pqs)
                ]

        ProjectDetailRoute.Settings ->
            Cmd.batch
                [ ProjectsApi.getSettings appState uuid GetSettingsCompleted
                , Task.dispatch InitSettings
                ]

        ProjectDetailRoute.Plugin _ ->
            ProjectsApi.get appState uuid GetCommonCompleted
    )



-- UPDATE


type Msg
    = GetCommonCompleted (Result ApiError ProjectCommon)
    | GetQuestionnaireCompleted (Result ApiError (ProjectDetailWrapper ProjectQuestionnaire))
    | GetPreviewCompleted (Result ApiError (ProjectDetailWrapper ProjectPreview))
    | GetSummaryReportCompleted (Result ApiError (ProjectDetailWrapper SummaryReport))
    | GetSettingsCompleted (Result ApiError (ProjectDetailWrapper ProjectSettings))
    | WebSocketMsg WebSocket.RawMsg
    | WebSocketPing
    | ActionsMenuMsg Int Menu.Msg
    | ShareModalMsg ShareModal.Msg
    | OpenVersionView Uuid String
    | VersionViewModalMsg VersionViewModal.Msg
    | OpenRevert ProjectEvent
    | RevertModalMsg RevertModal.Msg
    | ShareModalSaveMsg ShareModal.ShareData
    | CopyLink String
    | ClearCopiedLink
    | AddToMyProjects
    | PutShareCompleted (Result ApiError ())
    | TitleMsg EditableTitle.Msg
    | PutNameCompleted (Result ApiError ())
    | NoOp
    | PreviewMsg Preview.Msg
    | InitDocuments PaginationQueryString
    | DocumentsMsg Documents.Msg
    | InitNewDocument (Maybe Uuid)
    | NewDocumentMsg NewDocument.Msg
    | InitFiles PaginationQueryString
    | FilesMsg Files.Msg
    | InitSettings
    | SettingsMsg Settings.Msg
    | QuestionnaireMsg Questionnaire.Msg
    | ReplyDebounceMsg String Debounce.Msg
    | AddSavingEvent ProjectEvent
    | SavingIndicatorMsg SavingIndicator.Msg
    | CloseSaveError
    | Refresh


{-| The questionnaire's changes need the seed the event uuids came from, and are
sent over the socket as they are made; everything else is the page's own.
-}
update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Seed, Model, Cmd msg )
update cfg appState msg model =
    case msg of
        QuestionnaireMsg questionnaireMsg ->
            updateQuestionnaire cfg appState questionnaireMsg model

        ReplyDebounceMsg path debounceMsg ->
            let
                -- The reply goes, and only now is it saving: its uuid is what
                -- the server's echo is recognized by.
                send event =
                    Cmd.batch [ sendEvent model event, Task.dispatch (AddSavingEvent event) ]

                ( debounce, cmd ) =
                    Debounce.update (replyDebounceConfig appState path) (Debounce.takeLast send) debounceMsg (replyDebounce path model)
            in
            ( appState.seed, { model | replyDebounces = Dict.insert path debounce model.replyDebounces }, Cmd.map cfg.wrapMsg cmd )

        _ ->
            let
                ( newModel, cmd ) =
                    updatePage cfg appState msg model
            in
            ( appState.seed, newModel, cmd )


{-| Another tab of the project the model already holds, entered by a route: the
project and the socket stay (`init`'s request still refreshes the tab). The
questionnaire, when it is already built, follows a new deep link — the wizard
ignored one until the page was loaded afresh — or, without one, goes back to where
the reader left it.
-}
reenter : ProjectDetailRoute -> Model -> ( Model, Cmd Msg )
reenter route model =
    case route of
        ProjectDetailRoute.Questionnaire mbPath mbCommentThreadUuid ->
            ( { model | questionnaireLink = { path = mbPath, commentThreadUuid = mbCommentThreadUuid } }
            , if ActionResult.isSuccess model.questionnaireModel then
                case ( mbPath, mbCommentThreadUuid ) of
                    -- A link to a thread opens the comments on its question, as
                    -- it does when the project is opened by one; a link to a
                    -- question only goes there.
                    ( Just path, Just _ ) ->
                        Task.dispatch (QuestionnaireMsg (Questionnaire.openCommentsMsg path mbCommentThreadUuid))

                    ( Just path, Nothing ) ->
                        Task.dispatch (QuestionnaireMsg (Questionnaire.scrollToPathMsg True path))

                    _ ->
                        Task.dispatch (QuestionnaireMsg Questionnaire.restoreScrollMsg)

              else
                Cmd.none
            )

        _ ->
            ( model, Cmd.none )


{-| A change the user made in the questionnaire: already applied there, and sent
here. A reply is sent once typing in its question pauses (`replyDebounceConfig`,
as the wizard did); anything else at once. Each is saving until the server sends
it back, which is what the header and the guard go by.
-}
updateQuestionnaire : UpdateConfig Msg msg -> AppState -> Questionnaire.Msg -> Model -> ( Seed, Model, Cmd msg )
updateQuestionnaire cfg appState questionnaireMsg model =
    case ( model.common, model.questionnaireModel ) of
        ( Success common, Success questionnaireModel ) ->
            let
                result =
                    Questionnaire.update appState
                        { wrapMsg = cfg.wrapMsg << QuestionnaireMsg
                        , mbKmEditorUuid = Nothing
                        , previewVersionMsg = Just (\eventUuid title -> cfg.wrapMsg (OpenVersionView eventUuid title))
                        , revertVersionMsg = Just (cfg.wrapMsg << OpenRevert)
                        , projectCommon = common
                        }
                        questionnaireMsg
                        questionnaireModel

                newModel =
                    { model | questionnaireModel = Success result.model }

                savingMessage =
                    Window.setUnloadMessage (gettext "Some changes are still saving." appState.locale)
            in
            case result.event of
                Just ((ProjectEvent.SetReply _) as event) ->
                    let
                        path =
                            Maybe.withDefault "" (ProjectEvent.getPath event)

                        ( debounce, debounceCmd ) =
                            Debounce.push (replyDebounceConfig appState path) event (replyDebounce path newModel)
                    in
                    ( result.seed
                    , { newModel | replyDebounces = Dict.insert path debounce newModel.replyDebounces, saving = SavingIndicator.saving }
                    , Cmd.batch [ result.cmd, Cmd.map cfg.wrapMsg debounceCmd, savingMessage ]
                    )

                Just event ->
                    ( result.seed
                    , addSaving event newModel
                    , Cmd.batch [ result.cmd, sendEvent newModel event, savingMessage ]
                    )

                Nothing ->
                    ( result.seed, newModel, result.cmd )

        _ ->
            ( appState.seed, model, Cmd.none )


sendEvent : Model -> ProjectEvent -> Cmd msg
sendEvent model event =
    WebSocket.send model.websocket (ClientProjectMessage.encode (ClientProjectMessage.SetContent event))


addSaving : ProjectEvent -> Model -> Model
addSaving event model =
    { model | savingEventUuids = model.savingEventUuids ++ [ ProjectEvent.getUuid event ], saving = SavingIndicator.saving }


replyDebounceConfig : AppState -> String -> Debounce.Config Msg
replyDebounceConfig appState path =
    { strategy = Debounce.soon appState.websocketThrottleDelay
    , transform = ReplyDebounceMsg path
    }


replyDebounce : String -> Model -> Debounce ProjectEvent
replyDebounce path model =
    Maybe.withDefault Debounce.init (Dict.get path model.replyDebounces)


updatePage : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
updatePage cfg appState msg model =
    let
        -- The socket is opened by the first response to arrive, and only by
        -- it: after that the project is loaded and the socket is already open.
        openWebSocket =
            if ActionResult.isLoading model.common then
                WebSocket.open appState model.websocket

            else
                Cmd.none

        -- Whichever tab loaded the project, the shell hears which workspace it
        -- is in (`UpdateConfig.entityWorkspace`).
        loaded newModel =
            ( newModel
            , Cmd.batch
                [ openWebSocket
                , case newModel.common of
                    Success common ->
                        UpdateConfig.entityWorkspace cfg common.workspaceUuid

                    _ ->
                        Cmd.none
                ]
            )

        setError result error =
            let
                projectUrl =
                    Just (Routes.toUrl appState (Routes.projectsDetail model.uuid))
            in
            case ( error, Session.exists appState.session ) of
                ( BadStatus 403 _, False ) ->
                    ( model, Routes.cmdNavigateToLogin appState projectUrl )

                ( BadStatus 401 _, False ) ->
                    ( model, Routes.cmdNavigateToLogin appState projectUrl )

                ( BadStatus 401 _, True ) ->
                    ( model, Task.dispatch (cfg.logoutToLoginMsg projectUrl) )

                ( BadStatus 404 _, _ ) ->
                    ( model
                    , UpdateConfig.notFound cfg (NotFound.project appState)
                    )

                _ ->
                    ( { model | common = ApiError.toActionResult appState (gettext "Unable to get the project." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )
    in
    case msg of
        GetCommonCompleted result ->
            case result of
                Ok common ->
                    loaded { model | common = Success common }

                Err error ->
                    setError result error

        GetQuestionnaireCompleted result ->
            case result of
                Ok data ->
                    case model.questionnaireModel of
                        -- Back on the tab: the questionnaire the socket kept up
                        -- to date stays, with where the user was in it. Only
                        -- its files are taken from the response: a file deleted
                        -- from the files tab comes over no socket.
                        Success questionnaireModel ->
                            loaded
                                { model
                                    | questionnaire = Success data.data
                                    , questionnaireModel = Success (Questionnaire.setFiles data.data.files questionnaireModel)
                                    , common = Success data.common
                                }

                        _ ->
                            let
                                ( questionnaireModel, questionnaireCmd ) =
                                    Questionnaire.init appState data.data model.questionnaireLink.path model.questionnaireLink.commentThreadUuid

                                ( newModel, openCmd ) =
                                    loaded
                                        { model
                                            | questionnaire = Success data.data
                                            , questionnaireModel = Success questionnaireModel
                                            , common = Success data.common
                                        }
                            in
                            ( newModel, Cmd.batch [ openCmd, Cmd.map (cfg.wrapMsg << QuestionnaireMsg) questionnaireCmd ] )

                Err error ->
                    setError result error

        GetPreviewCompleted result ->
            case result of
                Ok data ->
                    -- Whether there is a document to preview at all is in the
                    -- project's own settings, so the tab is told what it is
                    -- looking at by the same response that fills the header.
                    loaded
                        { model
                            | common = Success data.common
                            , preview = Success data.data
                            , previewModel = Preview.setProject data.data model.previewModel
                        }

                Err error ->
                    setError result error

        GetSummaryReportCompleted result ->
            case result of
                Ok data ->
                    loaded { model | common = Success data.common, summaryReport = Success data.data }

                Err error ->
                    setError result error

        GetSettingsCompleted result ->
            case result of
                Ok data ->
                    -- The settings are what both the tabs asking for them are
                    -- made of: the settings form itself, and the new document
                    -- form, which starts from the project's name, the template
                    -- it was last given and that template's format and language.
                    loaded
                        { model
                            | common = Success data.common
                            , newDocument = NewDocument.setProject data.data model.newDocument
                            , settings = Settings.setProject appState data.data model.settings
                        }

                Err error ->
                    setError result error

        WebSocketMsg websocketMsg ->
            handleWebSocketMsg appState websocketMsg model

        SavingIndicatorMsg savingMsg ->
            ( { model | saving = SavingIndicator.update savingMsg model.saving }, Cmd.none )

        QuestionnaireMsg _ ->
            ( model, Cmd.none )

        ReplyDebounceMsg _ _ ->
            ( model, Cmd.none )

        AddSavingEvent event ->
            ( addSaving event model, Cmd.none )

        WebSocketPing ->
            ( model, WebSocket.ping model.websocket )

        ActionsMenuMsg count menuMsg ->
            let
                ( actionsMenu, menuCmd ) =
                    Menu.update actionsMenuId count menuMsg model.actionsMenu
            in
            ( { model | actionsMenu = actionsMenu }, Cmd.map (cfg.wrapMsg << ActionsMenuMsg count) menuCmd )

        OpenVersionView eventUuid title ->
            let
                ( versionViewModal, cmd ) =
                    VersionViewModal.init appState model.uuid eventUuid title
            in
            ( { model | versionViewModal = versionViewModal }, Cmd.map (cfg.wrapMsg << VersionViewModalMsg) cmd )

        VersionViewModalMsg modalMsg ->
            let
                ( versionViewModal, cmd ) =
                    VersionViewModal.update appState modalMsg model.versionViewModal
            in
            ( { model | versionViewModal = versionViewModal }, Cmd.map (cfg.wrapMsg << VersionViewModalMsg) cmd )

        OpenRevert event ->
            ( { model | revertModal = RevertModal.setEvent event model.revertModal }, Cmd.none )

        RevertModalMsg modalMsg ->
            let
                ( revertModal, cmd ) =
                    RevertModal.update appState { projectUuid = model.uuid } modalMsg model.revertModal
            in
            ( { model | revertModal = revertModal }, Cmd.map (cfg.wrapMsg << RevertModalMsg) cmd )

        ShareModalMsg shareModalMsg ->
            let
                ( shareModal, cmd ) =
                    ShareModal.update
                        { wrapMsg = cfg.wrapMsg << ShareModalMsg
                        , logoutMsg = cfg.logoutMsg
                        , projectUuid = model.uuid
                        , onSaveMsg = cfg.wrapMsg << ShareModalSaveMsg
                        }
                        appState
                        shareModalMsg
                        model.shareModal
            in
            ( { model | shareModal = shareModal }, cmd )

        ShareModalSaveMsg shareData ->
            -- The header says who can reach the project, so it is told what was
            -- just saved rather than asking for the project again.
            ( { model
                | common = ActionResult.map (ProjectCommon.updateWithShareData shareData) model.common
                , questionnaireModel = ActionResult.map (Questionnaire.updateWithShareData shareData) model.questionnaireModel
              }
            , Cmd.none
            )

        CopyLink link ->
            -- The menu the action was in has closed by the time the link is on
            -- the clipboard, so the only way to say so is to announce it.
            ( { model | copiedLink = True }
            , Cmd.batch
                [ Copy.copyToClipboard link
                , Cmd.map cfg.wrapMsg (Task.dispatchAfter copiedLinkFor ClearCopiedLink)
                ]
            )

        ClearCopiedLink ->
            ( { model | copiedLink = False }, Cmd.none )

        AddToMyProjects ->
            case model.common of
                Success common ->
                    -- Taking on a project that belongs to nobody: the same
                    -- share request the dialog makes, with the current user as
                    -- its only member and its owner.
                    let
                        owner =
                            { member =
                                Member.userMember
                                    { uuid = Maybe.unwrap Uuid.nil .uuid appState.config.user
                                    , firstName = ""
                                    , lastName = ""
                                    , gravatarHash = ""
                                    , imageUrl = Nothing
                                    , affiliation = Nothing
                                    }
                            , perms = ProjectPerm.all
                            }
                    in
                    case Form.getOutput (ProjectShareForm.init { common | permissions = [ owner ] }) of
                        Just form ->
                            ( { model | addingToMyProjects = Loading }
                            , ProjectsApi.putShare appState
                                model.uuid
                                (ProjectShareForm.encode form)
                                (cfg.wrapMsg << PutShareCompleted)
                            )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PutShareCompleted result ->
            case result of
                Ok _ ->
                    -- Everything on the page was loaded by somebody with no
                    -- permissions on it; now there are some, so it is loaded
                    -- again.
                    ( { model | addingToMyProjects = Success () }, Window.refresh () )

                Err error ->
                    ( { model
                        | addingToMyProjects = Unset
                        , saveError = Just (apiSaveErrorOf appState (gettext "The project could not be added to your projects." appState.locale) error)
                      }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        TitleMsg titleMsg ->
            case model.common of
                Success common ->
                    let
                        ( title, titleCmd, outcome ) =
                            EditableTitle.update common.name titleMsg model.title

                        withTitle =
                            { model | title = title }

                        titleCmdMapped =
                            Cmd.map (cfg.wrapMsg << TitleMsg) titleCmd
                    in
                    case outcome of
                        -- The header says the new name at once; the page is not
                        -- reloaded, as it is after the settings are saved,
                        -- because nothing else on it depends on the name.
                        EditableTitle.Renamed name ->
                            ( rename appState name { withTitle | renamingFrom = Just common.name }
                            , Cmd.batch
                                [ titleCmdMapped
                                , ProjectsApi.putName appState model.uuid name (cfg.wrapMsg << PutNameCompleted)
                                ]
                            )

                        EditableTitle.Unchanged ->
                            ( withTitle, titleCmdMapped )

                _ ->
                    ( model, Cmd.none )

        PutNameCompleted result ->
            case ( result, model.renamingFrom ) of
                ( Err error, Just previous ) ->
                    ( rename appState
                        previous
                        { model
                            | renamingFrom = Nothing
                            , saveError = Just (apiSaveErrorOf appState (gettext "The project could not be renamed." appState.locale) error)
                        }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

                _ ->
                    ( { model | renamingFrom = Nothing }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        PreviewMsg previewMsg ->
            let
                ( previewModel, cmd ) =
                    Preview.update (UpdateConfig.map PreviewMsg cfg) appState model.uuid previewMsg model.previewModel
            in
            ( { model | previewModel = previewModel }, cmd )

        InitDocuments pqs ->
            let
                ( documents, cmd ) =
                    Documents.init pqs
            in
            ( { model | documents = documents }, Cmd.map (cfg.wrapMsg << DocumentsMsg) cmd )

        DocumentsMsg documentsMsg ->
            let
                ( documents, cmd ) =
                    Documents.update (UpdateConfig.map DocumentsMsg cfg)
                        { onViewQuestionnaire = \eventUuid title -> cfg.wrapMsg (OpenVersionView eventUuid title) }
                        appState
                        model.uuid
                        documentsMsg
                        model.documents
            in
            ( { model | documents = documents }, cmd )

        InitNewDocument mbEventUuid ->
            let
                ( newDocument, cmd ) =
                    NewDocument.init appState model.uuid mbEventUuid
            in
            ( { model | newDocument = newDocument }, Cmd.map (cfg.wrapMsg << NewDocumentMsg) cmd )

        NewDocumentMsg newDocumentMsg ->
            case model.common of
                Success common ->
                    let
                        ( newDocument, cmd ) =
                            NewDocument.update (UpdateConfig.map NewDocumentMsg cfg)
                                appState
                                { projectUuid = model.uuid
                                , knowledgeModelPackageUuid = common.knowledgeModelPackage.uuid
                                }
                                newDocumentMsg
                                model.newDocument
                    in
                    ( { model | newDocument = newDocument }, cmd )

                _ ->
                    ( model, Cmd.none )

        InitFiles pqs ->
            let
                ( files, cmd ) =
                    Files.init pqs
            in
            ( { model | files = files }, Cmd.map (cfg.wrapMsg << FilesMsg) cmd )

        FilesMsg filesMsg ->
            let
                ( files, cmd ) =
                    Files.update (UpdateConfig.map FilesMsg cfg) appState model.uuid filesMsg model.files
            in
            ( { model | files = files }, cmd )

        InitSettings ->
            ( { model | settings = Settings.initialModel appState }, Cmd.none )

        SettingsMsg settingsMsg ->
            let
                ( settings, cmd ) =
                    Settings.update (UpdateConfig.map SettingsMsg cfg) appState model.uuid settingsMsg model.settings
            in
            ( { model | settings = settings }, cmd )

        CloseSaveError ->
            ( { model | saveError = Nothing }, Cmd.none )

        Refresh ->
            ( model, Window.refresh () )


{-| The project is called `name` now: in the header, and in the settings tab
unless its own name field has been typed over.
-}
rename : AppState -> String -> Model -> Model
rename appState name model =
    { model
        | common = ActionResult.map (\common -> { common | name = name }) model.common
        , settings = Settings.setName appState name model.settings
    }


actionsMenuId : String
actionsMenuId =
    "project-actions"


{-| How long "Copied!" is announced for — as long as `Ui.Copy` says it, which
is what says it everywhere else.
-}
copiedLinkFor : Float
copiedLinkFor =
    2000


{-| The project as somebody outside the application would open it: the client's
own address and the project's route, which is what the public link is.
-}
projectLink : AppState -> Uuid -> String
projectLink appState uuid =
    AppState.getClientUrlRoot appState ++ Routes.toUrl appState (Routes.projectsDetail uuid)


{-| What the server says: who else is here, the project's data, a change to the
questionnaire — ours coming back, which ends its saving, or somebody else's, which
is applied — a file uploaded, or that the socket is done.
-}
handleWebSocketMsg : AppState -> WebSocket.RawMsg -> Model -> ( Model, Cmd msg )
handleWebSocketMsg appState websocketMsg model =
    case WebSocket.receive (WebSocketServerAction.decoder ServerProjectMessage.decoder) websocketMsg model.websocket of
        WebSocket.Message serverAction ->
            case serverAction of
                WebSocketServerAction.Success message ->
                    case message of
                        ServerProjectMessage.SetUserList users ->
                            ( { model | onlineUsers = users }, Cmd.none )

                        ServerProjectMessage.SetProject data ->
                            ( { model
                                | common = ActionResult.map (ProjectCommon.updateWithQuestionnaireData data) model.common
                                , questionnaireModel = ActionResult.map (Questionnaire.updateWithQuestionnaireData data) model.questionnaireModel
                                , settings = Settings.setName appState data.name model.settings
                              }
                            , Cmd.none
                            )

                        ServerProjectMessage.SetContent event ->
                            let
                                eventUuid =
                                    ProjectEvent.getUuid event
                            in
                            if List.member eventUuid model.savingEventUuids then
                                let
                                    remaining =
                                        List.filter ((/=) eventUuid) model.savingEventUuids
                                in
                                if List.isEmpty remaining then
                                    ( { model | savingEventUuids = [], saving = SavingIndicator.saved }, Window.clearUnloadMessage () )

                                else
                                    ( { model | savingEventUuids = remaining }, Cmd.none )

                            else
                                ( { model | questionnaireModel = ActionResult.map (Questionnaire.applyProjectEvent event) model.questionnaireModel }
                                , Cmd.none
                                )

                        ServerProjectMessage.AddFile file ->
                            ( { model | questionnaireModel = ActionResult.map (Questionnaire.addFile file) model.questionnaireModel }, Cmd.none )

                WebSocketServerAction.Error ((ServerError.UserSimpleError message) as serverError) ->
                    if message.code == forceDisconnectCode then
                        ( { model | forceDisconnect = True }, Cmd.none )

                    else
                        ( savingFailed appState serverError model, Cmd.none )

                WebSocketServerAction.Error serverError ->
                    ( savingFailed appState serverError model, Cmd.none )

        WebSocket.Reconnect ->
            ( model, WebSocket.open appState model.websocket )

        WebSocket.Close ->
            ( { model | offline = True }, Cmd.none )

        _ ->
            ( model, Cmd.none )


forceDisconnectCode : String
forceDisconnectCode =
    "error.service.project.collaboration.force_disconnect"


{-| Why something on the project could not be saved, for the dialog that says
so (`saveErrorModal`). `stale` is whether what the page shows may no longer be
what the server has, so the way on is a refresh.
-}
type alias SaveError =
    { message : String
    , details : List String
    , stale : Bool
    }


{-| The server refused a change sent over the socket. It does not say which
one, so the changes still waiting stay waiting; the indicator just stops saying
"Saving..." for one that is not coming. The questionnaire stays on the screen,
to be looked at or copied, with the error in a dialog over it.
-}
savingFailed : AppState -> ServerError.ServerError -> Model -> Model
savingFailed appState serverError model =
    { model
        | saveError = Just (saveErrorOf appState True (gettext "The change could not be saved." appState.locale) serverError)
        , saving = SavingIndicator.init
    }


{-| A request that failed. A refused one says why in its body, which is read
even for a 403, whose reason (what the user is not allowed to do) is more use
than that they are not allowed something. The request changed nothing, so
what is on the screen is still what is saved.
-}
apiSaveErrorOf : AppState -> String -> ApiError -> SaveError
apiSaveErrorOf appState defaultMessage error =
    let
        serverError =
            case error of
                BadStatus 403 body ->
                    Maybe.or (Result.toMaybe (D.decodeString ServerError.decoder body)) (Just ServerError.ForbiddenError)

                _ ->
                    ApiError.toServerError error
    in
    case serverError of
        Just refused ->
            saveErrorOf appState False defaultMessage refused

        Nothing ->
            { message = defaultMessage, details = [], stale = False }


{-| What the server said, in the user's language when it is a message we know
and in the server's own words when it is not (`ServerError.messageToText`);
our own sentence only when the server said nothing usable.
-}
saveErrorOf : AppState -> Bool -> String -> ServerError.ServerError -> SaveError
saveErrorOf appState stale defaultMessage serverError =
    let
        simple message =
            { message = message, details = [], stale = stale }

        text message =
            Maybe.withDefault message.code (ServerError.messageToText appState message)
    in
    case serverError of
        ServerError.UserSimpleError message ->
            simple (Maybe.withDefault defaultMessage (ServerError.messageToText appState message))

        ServerError.UserFormError data ->
            { message = gettext "The server rejected the change." appState.locale
            , details =
                List.map text data.formErrors
                    ++ List.map text (List.concat (Dict.values data.fieldErrors))
            , stale = stale
            }

        ServerError.SystemLogError data ->
            simple (String.format data.defaultMessage data.params)

        ServerError.ForbiddenError ->
            simple (gettext "You do not have permission to do this." appState.locale)

        ServerError.TooManyRequestsError data ->
            simple (ServerError.tooManyRequestsMessage appState data)



-- SUBSCRIPTIONS


{-| The tab's own subscriptions are the tab's own: the documents listing polls
while a document is being generated, and it should not go on polling from
behind another tab.
-}
subscriptions : AppState -> ProjectDetailRoute -> Model -> Sub Msg
subscriptions appState route model =
    Sub.batch
        [ WebSocket.listen WebSocketMsg
        , WebSocket.schedulePing WebSocketPing
        , Sub.map (ActionsMenuMsg 0) (Menu.subscriptions actionsMenuId model.actionsMenu)
        , Sub.map ShareModalMsg (ShareModal.subscriptions model.shareModal)
        , Sub.map VersionViewModalMsg (VersionViewModal.subscriptions model.versionViewModal)
        , Sub.map SavingIndicatorMsg (SavingIndicator.subscriptions model.saving)
        , case route of
            ProjectDetailRoute.Questionnaire _ _ ->
                case model.questionnaireModel of
                    Success questionnaireModel ->
                        Sub.map QuestionnaireMsg (Questionnaire.subscriptions questionnaireModel)

                    _ ->
                        Sub.none

            ProjectDetailRoute.Preview ->
                Sub.map PreviewMsg (Preview.subscriptions model.previewModel)

            ProjectDetailRoute.Documents _ ->
                Sub.map DocumentsMsg (Documents.subscriptions model.documents)

            ProjectDetailRoute.NewDocument _ ->
                Sub.map NewDocumentMsg (NewDocument.subscriptions appState model.newDocument)

            ProjectDetailRoute.Files _ ->
                Sub.map FilesMsg (Files.subscriptions model.files)

            ProjectDetailRoute.Settings ->
                Sub.map SettingsMsg (Settings.subscriptions appState model.settings)

            _ ->
                Sub.none
        ]



-- GUARD


{-| Changes still on their way to the server keep the user on the project —
another tab of it is fine, the socket stays open — as the wizard did. Otherwise
the tab decides: the settings form is the one thing on this page that is edited
and then saved, so leaving it with something typed asks first.
-}
isGuarded : AppState -> ProjectDetailRoute -> Maybe Routes.Route -> Model -> Maybe String
isGuarded appState route mbNextRoute model =
    let
        stayingOnProject =
            Maybe.unwrap False (Routes.isProjectsDetail model.uuid) mbNextRoute
    in
    -- Saving covers a reply still waiting for typing to pause, before it is
    -- sent: leaving then would lose it.
    if (SavingIndicator.isSaving model.saving || not (List.isEmpty model.savingEventUuids)) && not stayingOnProject then
        Just (gettext "Some changes are still saving." appState.locale)

    else
        case route of
            ProjectDetailRoute.Settings ->
                Settings.isGuarded appState model.settings

            _ ->
                Nothing



-- VIEW


view : AppState -> ProjectDetailRoute -> Model -> Html Msg
view appState route model =
    if model.offline then
        viewOffline appState

    else
        DetailPage.view appState (viewProject appState route model) model.common


{-| The socket closed. Everything on the page is as stale as the moment it
went, so there is nothing to do here but load the page again.
-}
viewOffline : AppState -> Html Msg
viewOffline appState =
    Message.message
        { title = gettext "Disconnected" appState.locale
        , description = gettext "You have been disconnected, try to refresh the page." appState.locale
        }
        |> Message.withIllustration (Illustration.toHtml Illustration.error)
        |> Message.withAction (Button.primary (gettext "Refresh" appState.locale) Refresh)
        |> Message.toHtml


viewProject : AppState -> ProjectDetailRoute -> Model -> ProjectCommon -> DetailPage.DetailPage Msg
viewProject appState route model common =
    let
        badges =
            if common.isTemplate then
                [ Badge.info (gettext "Template" appState.locale) ]

            else
                []

        flushContent =
            isFlushContent appState route model
    in
    DetailPage.detailPage
        { title = common.name
        , content =
            content appState route model common
                ++ [ disconnectedModal appState model
                   , saveErrorModal appState model
                   , Html.map ShareModalMsg (ShareModal.view appState model.shareModal)
                   , Html.map VersionViewModalMsg (VersionViewModal.view appState model.versionViewModal)
                   , Html.map RevertModalMsg (RevertModal.view appState model.revertModal)
                   , Text.announcement (copiedLinkAnnouncement appState model)
                   ]
        }
        |> DetailPage.withFlushContent flushContent
        |> withFooter appState route model common
        |> DetailPage.withBadges badges
        |> withOnlineUsers appState model
        |> withActions appState model common
        |> withEditableTitle appState model common
        |> DetailPage.withTabsWithTestIds
            { label = gettext "Project" appState.locale
            , items = tabs appState route model common
            }


{-| The band under the tab's content, for the tab that saves something: the
settings, whose unsaved-changes bar goes there as it does on every settings
screen. No other tab has one, and neither does a reader the settings are not
shown to.
-}
withFooter : AppState -> ProjectDetailRoute -> Model -> ProjectCommon -> DetailPage.DetailPage Msg -> DetailPage.DetailPage Msg
withFooter appState route model common =
    if route == ProjectDetailRoute.Settings && Feature.projectSettings appState common && Session.exists appState.session then
        case Settings.footer appState model.settings of
            Just footer ->
                DetailPage.withFooter (Html.map SettingsMsg footer)

            Nothing ->
                identity

    else
        identity


{-| Whether the tab is one surface edge to edge
(`DetailPage.withFlushContent`): the preview, once what it shows is the
document in its frame, and the questionnaire, once it is built — a workspace
with scrolling regions of its own. While either loads, or when it has something
to say, the body stays padded so the loader and the message have room round
them.
-}
isFlushContent : AppState -> ProjectDetailRoute -> Model -> Bool
isFlushContent appState route model =
    case route of
        ProjectDetailRoute.Preview ->
            ActionResult.isSuccess model.preview && Preview.isDocumentShown appState model.previewModel

        ProjectDetailRoute.Questionnaire _ _ ->
            ActionResult.isSuccess model.questionnaireModel

        -- A plugin's tab is the plugin's: it gets the tab edge to edge, and what
        -- is in it is not ours to pad. Not when the plugin is missing, which is
        -- a message of ours and wants the room round it.
        ProjectDetailRoute.Plugin pluginId ->
            case model.common of
                Success common ->
                    List.any (\( _, connector ) -> connector.url == pluginId)
                        (projectTabPlugins appState common)

                _ ->
                    False

        _ ->
            False


{-| Another user changed the project so much that everyone's copy is stale;
the server disconnected us rather than let the page drift. There is no way on
from here other than loading the page again, so closing the dialog does that
too.
-}
disconnectedModal : AppState -> Model -> Html Msg
disconnectedModal appState model =
    ConfirmModal.confirmWithTestId appState
        { title = gettext "Refresh Required" appState.locale
        , open = model.forceDisconnect
        , body =
            [ Layout.stack
                [ Text.body (gettext "Another user has made significant changes to the questionnaire (e.g., reverting to a previous version). To ensure everyone is using the latest version, you’ve been disconnected." appState.locale)
                , Text.body (gettext "Refresh the page to reconnect." appState.locale)
                ]
            ]
        , confirmLabel = gettext "Refresh" appState.locale
        , onConfirm = Refresh
        , onClose = Refresh
        , result = Unset
        , testId = Just "disconnected_modal"
        }


{-| Why something on the project could not be saved, in a dialog rather than
instead of the page or in an alert under it: what is on the screen stays there
to be looked at or copied, and the error is not missed below a long
questionnaire. When what is on the screen may no longer be what the server
has, the way on is a refresh.
-}
saveErrorModal : AppState -> Model -> Html Msg
saveErrorModal appState model =
    let
        stale =
            Maybe.unwrap False .stale model.saveError

        refreshAction =
            if stale then
                [ Button.primary (gettext "Refresh" appState.locale) Refresh
                    |> Button.withTestId "project_detail_save-error-refresh"
                ]

            else
                []
    in
    Modal.plain
        { title = gettext "Unable to save" appState.locale
        , open = Maybe.isJust model.saveError
        , onClose = CloseSaveError
        , body =
            case model.saveError of
                Just saveError ->
                    (Alert.danger ""
                        |> Alert.toHtmlWithContent
                            (Html.p [] [ Html.strong [] [ Html.text saveError.message ] ]
                                :: List.map (\detail -> Html.p [] [ Html.text detail ]) saveError.details
                            )
                    )
                        :: (if saveError.stale then
                                [ Html.p [] [ Html.text (gettext "The project no longer shows what is saved. Refresh the page to continue with the saved project." appState.locale) ] ]

                            else
                                []
                           )

                Nothing ->
                    []
        }
        |> Modal.withActions
            (Button.ghost (gettext "Close" appState.locale) CloseSaveError
                :: refreshAction
            )
        |> Modal.withTestId "project_detail_save-error-modal"
        |> Modal.toHtml


{-| The name can be changed from the title by whoever can change it in the
settings: the project's owner, signed in.
-}
withEditableTitle : AppState -> Model -> ProjectCommon -> DetailPage.DetailPage Msg -> DetailPage.DetailPage Msg
withEditableTitle appState model common =
    if Feature.projectSettings appState common && Session.exists appState.session then
        DetailPage.withEditableTitle appState
            { model = model.title
            , toMsg = TitleMsg
            , label = gettext "Project name" appState.locale
            , saving = Maybe.isJust model.renamingFrom
            , testId = "project_detail_title"
            }

    else
        identity



-- VIEW - HEADER ACTIONS


{-| What the header offers, which depends on who is looking.

The wizard had one split button: Share, with a dropdown holding "Copy link" and,
below a divider, the sentence saying who can reach the project. It is one here
too — Share is the header's button (`DetailPage.withActions`), and the rest is
the menu joined to it (`withMoreActions`, `withMoreActionsJoined`), where the
sentence is a note rather than something to press. Joined rather than a kebab
of its own, because everything in it is more of sharing, not the project's
overflow.

The Share button carries the sharing icon the wizard put on it, so how the
project is shared is visible before the menu is opened. It is secondary:
sharing is something done now and then, and a filled button in the header drew
the eye from the questionnaire the page is for. "Add to my projects" stays
primary — for a visitor to someone else's anonymous project, it is the action.

-}
withActions : AppState -> Model -> ProjectCommon -> DetailPage.DetailPage Msg -> DetailPage.DetailPage Msg
withActions appState model common detailPage =
    if ProjectUtils.isAnonymousProject common && Session.exists appState.session then
        DetailPage.withActions
            [ Button.primary (gettext "Add to my projects" appState.locale) AddToMyProjects
                |> Button.withIcon (Icon.solid "fa-plus")
                |> Button.withLoading (ActionResult.isLoading model.addingToMyProjects)
                |> Button.withTestId "project_detail_add-to-my-projects"
            ]
            detailPage

    else if ProjectUtils.isOwner appState common then
        let
            actions =
                shareActions appState model common
        in
        detailPage
            |> DetailPage.withActions
                [ Button.secondary (gettext "Share" appState.locale) (ShareModalMsg (ShareModal.open common))
                    |> Button.withIcon (ProjectSharing.icon common)
                    |> Button.withTestId "project_detail_share-button"
                ]
            |> DetailPage.withMoreActions
                { id = actionsMenuId
                , label = gettext "Sharing options" appState.locale
                , actions = actions
                , model = model.actionsMenu
                , toMsg = ActionsMenuMsg (ItemActions.focusableCount actions)
                }
            |> DetailPage.withMoreActionsJoined (gettext "Sharing" appState.locale)

    else
        detailPage


shareActions : AppState -> Model -> ProjectCommon -> List (ItemActions.Action Msg)
shareActions appState model common =
    ItemActions.groups
        [ [ ( ItemActions.action
                { icon = Icon.solid "fa-link"
                , label = gettext "Copy link" appState.locale
                , onSelect = CopyLink (projectLink appState model.uuid)
                }
                |> ItemActions.withTestId "project_detail_copy-link"
            , True
            )
          ]
        , [ ( ItemActions.note [ Text.muted (ProjectSharing.description appState common) ], True ) ]
        ]


copiedLinkAnnouncement : AppState -> Model -> String
copiedLinkAnnouncement appState model =
    if model.copiedLink then
        gettext "Copied!" appState.locale

    else
        ""



-- VIEW - ONLINE USERS


{-| Who else has this project open, beside its name. The current user is not
one of them — they know they are here — and a user with two tabs open is one
face, as they were in the wizard. Anonymous visitors have no identity to
compare, so each of them counts once.
-}
withOnlineUsers : AppState -> Model -> DetailPage.DetailPage Msg -> DetailPage.DetailPage Msg
withOnlineUsers appState model detailPage =
    let
        -- The people here now go before the actions, apart from them by a rule,
        -- as in the wizard; the saving status stays by the title.
        withAvatars =
            case OnlineUsers.avatars appState model.onlineUsers of
                Just group ->
                    DetailPage.withActionsLeading group

                Nothing ->
                    identity

        withSaving =
            case savingIndicator appState model of
                [] ->
                    identity

                saving ->
                    DetailPage.withHeaderAside (Layout.inline saving)
    in
    detailPage
        |> withAvatars
        |> withSaving


{-| Where the user's changes to the questionnaire are, beside the people online:
saving, saved a moment ago, or all saved. A status, said politely as it changes,
and never a control — the header's line belongs to the heading. Shown to whoever
can change the questionnaire, once it is loaded.
-}
savingIndicator : AppState -> Model -> List (Html Msg)
savingIndicator appState model =
    case ( model.questionnaireModel, model.common ) of
        ( Success _, Success common ) ->
            if ProjectUtils.isEditor appState common then
                [ SavingIndicator.toHtml appState model.saving ]

            else
                []

        _ ->
            []



-- VIEW - TABS


type alias Tab =
    { label : String
    , url : String
    , active : Bool
    , badge : Maybe String
    , testId : Maybe String
    , icon : Maybe Icon.Icon
    , visible : Bool
    }


tabs : AppState -> ProjectDetailRoute -> Model -> ProjectCommon -> List DetailPage.TabItem
tabs appState route model common =
    let
        projectUrl subroute =
            Routes.toUrl appState (Routes.ProjectsRoute (ProjectsRoutes.DetailRoute model.uuid subroute))

        questionnaireTab =
            { label = gettext "Questionnaire" appState.locale
            , url = projectUrl (ProjectDetailRoute.Questionnaire Nothing Nothing)
            , active = isQuestionnaireRoute route
            , badge = Nothing
            , testId = Just "project_nav_questionnaire"
            , icon = Just (Icon.regular "fa-rectangle-list")
            , visible = True
            }

        metricsTab =
            { label = gettext "Metrics" appState.locale
            , url = projectUrl ProjectDetailRoute.Metrics
            , active = route == ProjectDetailRoute.Metrics
            , badge = Nothing
            , testId = Just "project_nav_metrics"
            , icon = Just (Icon.regular "fa-chart-bar")
            , visible = Feature.projectMetrics appState
            }

        previewTab =
            { label = gettext "Preview" appState.locale
            , url = projectUrl ProjectDetailRoute.Preview
            , active = route == ProjectDetailRoute.Preview
            , badge = Nothing
            , testId = Just "project_nav_preview"
            , icon = Just (Icon.solid "fa-eye")
            , visible = Feature.projectPreview
            }

        documentsTab =
            { label = gettext "Documents" appState.locale
            , url = projectUrl (ProjectDetailRoute.Documents PaginationQueryString.empty)
            , active = isDocumentsRoute route
            , badge = Nothing
            , testId = Just "project_nav_documents"
            , icon = Just (Icon.regular "fa-copy")
            , visible = Feature.projectDocumentsView
            }

        filesTab =
            { label = gettext "Files" appState.locale
            , url = projectUrl (ProjectDetailRoute.Files PaginationQueryString.empty)
            , active = isFilesRoute route
            , badge = Nothing
            , testId = Just "project_nav_files"
            , icon = Just (Icon.regular "fa-folder-closed")

            -- The count is the project's as it was loaded; a file uploaded
            -- since is in the questionnaire, which the socket keeps current.
            , visible = common.fileCount > 0 || ActionResult.unwrap False Questionnaire.hasFiles model.questionnaireModel
            }

        settingsTab =
            { label = gettext "Settings" appState.locale
            , url = projectUrl ProjectDetailRoute.Settings
            , active = route == ProjectDetailRoute.Settings
            , badge = Nothing
            , testId = Just "project_nav_settings"
            , icon = Just (Icon.solid "fa-gears")
            , visible = Feature.projectSettings appState common
            }

        pluginTab ( _, connector ) =
            { label = gettext connector.tab.name appState.locale
            , url = projectUrl (ProjectDetailRoute.Plugin connector.url)
            , active = route == ProjectDetailRoute.Plugin connector.url
            , badge = Nothing
            , testId = Just ("project_nav_plugin_" ++ connector.url)

            -- The icon the plugin gave its tab, as every other tab here has
            -- one. Nothing rather than the question mark `Icon.fromClass` falls
            -- back to when a plugin names none: a tab with no icon reads as a
            -- tab, a tab with a question mark reads as a mistake.
            , icon =
                if String.isEmpty (String.trim connector.tab.icon) then
                    Nothing

                else
                    Just (Icon.fromClass connector.tab.icon)
            , visible = True
            }

        pluginTabs =
            projectTabPlugins appState common
                |> List.sortBy (.name << .tab << Tuple.second)
                |> List.map pluginTab
    in
    ([ questionnaireTab
     , metricsTab
     , previewTab
     , documentsTab
     , filesTab
     ]
        ++ pluginTabs
        ++ [ settingsTab ]
    )
        |> List.filter .visible
        |> List.map toTabItem


toTabItem : Tab -> DetailPage.TabItem
toTabItem tab =
    { label = tab.label
    , url = tab.url
    , active = tab.active
    , badge = tab.badge
    , testId = tab.testId
    , icon = tab.icon
    }


isQuestionnaireRoute : ProjectDetailRoute -> Bool
isQuestionnaireRoute route =
    case route of
        ProjectDetailRoute.Questionnaire _ _ ->
            True

        _ ->
            False


{-| Creating a document is part of the documents tab, so the tab stays current
while it is open.
-}
isDocumentsRoute : ProjectDetailRoute -> Bool
isDocumentsRoute route =
    case route of
        ProjectDetailRoute.Documents _ ->
            True

        ProjectDetailRoute.NewDocument _ ->
            True

        _ ->
            False


isFilesRoute : ProjectDetailRoute -> Bool
isFilesRoute route =
    case route of
        ProjectDetailRoute.Files _ ->
            True

        _ ->
            False


{-| The plugin tabs this project's knowledge model matches.
-}
projectTabPlugins : AppState -> ProjectCommon -> List ( Plugin, ProjectTabConnector )
projectTabPlugins appState common =
    AppState.getPluginsByConnector appState .projectTabs
        |> Plugin.filterByKmPatterns (KnowledgeModelUtils.getPackageId common.knowledgeModelPackage)



-- VIEW - CONTENT


content : AppState -> ProjectDetailRoute -> Model -> ProjectCommon -> List (Html Msg)
content appState route model common =
    case route of
        ProjectDetailRoute.Questionnaire _ _ ->
            -- Inside the tab, under the page's `h1`: loading and errors are the
            -- tab's own, and the questionnaire's chapter is an `h2`.
            [ Page.actionResultWithinPage appState
                (\questionnaireModel ->
                    Questionnaire.view appState
                        { wrapMsg = QuestionnaireMsg
                        , readonly = not (ProjectUtils.isEditor appState common)
                        , commentsReadonly = not (ProjectUtils.canComment appState common)
                        , actionsEnabled = True
                        , toolbarEnabled = True
                        , canViewVersions = True
                        , canRevert = ProjectUtils.isOwner appState common
                        }
                        questionnaireModel
                )
                model.questionnaireModel
            ]

        ProjectDetailRoute.Metrics ->
            [ Page.actionResult appState (Metrics.view appState) model.summaryReport ]

        ProjectDetailRoute.Preview ->
            -- Inside the tab, under the page's `h1`: the error is a message
            -- with an `h2`, not a second page.
            [ Html.map PreviewMsg (Page.actionResultWithinPage appState (\preview -> Preview.view appState preview model.previewModel) model.preview) ]

        ProjectDetailRoute.Documents _ ->
            [ Html.map DocumentsMsg (Documents.view appState common model.documents) ]

        ProjectDetailRoute.NewDocument _ ->
            -- Adding a document changes the project, so it is for an editor who
            -- is signed in. An alert rather than a whole error page: the project
            -- around it loaded, and the page already has its heading.
            if ProjectUtils.isEditor appState common && Session.exists appState.session then
                [ Html.map NewDocumentMsg (NewDocument.view appState common.uuid model.newDocument) ]

            else
                [ Alert.toHtml (Alert.danger (gettext "You are not allowed to view this page." appState.locale)) ]

        ProjectDetailRoute.Files _ ->
            [ Html.map FilesMsg (Files.view appState common model.files) ]

        ProjectDetailRoute.Settings ->
            -- The settings belong to whoever owns the project and is signed in,
            -- as the tab itself is. An alert rather than a whole error page:
            -- the project around it loaded, and the page already has its
            -- heading.
            if Feature.projectSettings appState common && Session.exists appState.session then
                [ Html.map SettingsMsg (Settings.view appState model.settings) ]

            else
                [ Alert.toHtml (Alert.danger (gettext "You are not allowed to view this page." appState.locale)) ]

        ProjectDetailRoute.Plugin pluginId ->
            pluginTabContent appState common pluginId


pluginTabContent : AppState -> ProjectCommon -> String -> List (Html Msg)
pluginTabContent appState common pluginId =
    case List.find (\( _, connector ) -> connector.url == pluginId) (projectTabPlugins appState common) of
        Just ( plugin, connector ) ->
            [ div [ class "c-project-plugin-tab" ]
                [ PluginView.view appState plugin.uuid connector.element [ PluginElement.projectValue common ] ]
            ]

        Nothing ->
            -- An alert and not a whole error page: the project around it
            -- loaded, and the page already has its heading.
            [ Alert.toHtml (Alert.danger (gettext "Plugin not found" appState.locale)) ]


{-| What the page is about, by name, for the window's title, once it has
arrived.
-}
entityName : Model -> Maybe String
entityName model =
    Maybe.map .name (ActionResult.toMaybe model.common)
