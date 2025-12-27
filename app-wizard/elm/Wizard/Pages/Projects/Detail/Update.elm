module Wizard.Pages.Projects.Detail.Update exposing
    ( fetchData
    , isGuarded
    , onUnload
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError(..))
import Common.Api.Models.WebSockets.WebSocketServerAction as WebSocketServerAction
import Common.Api.WebSocket as WebSocket
import Common.Ports.Copy as Ports
import Common.Ports.Window as Window
import Common.Utils.Driver as Driver exposing (TourConfig)
import Common.Utils.RequestHelpers as RequestHelpers
import Debounce
import Dict
import Form
import Gettext exposing (gettext)
import Html.Attributes.Extensions exposing (selectDataTour)
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Task.Extra as Task
import Triple
import Uuid exposing (Uuid)
import Wizard.Api.Models.BootstrapConfig.UserConfig as UserConfig
import Wizard.Api.Models.Member as Member
import Wizard.Api.Models.ProjectCommon as ProjectCommon
import Wizard.Api.Models.ProjectDetail.CommentThread as CommentThread
import Wizard.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.AddCommentData as AddCommentData
import Wizard.Api.Models.ProjectDetail.ProjectEvent.SetReplyData as SetReplyData
import Wizard.Api.Models.ProjectPerm as ProjectPerm
import Wizard.Api.Models.ProjectPreview as ProjectPreview
import Wizard.Api.Models.WebSockets.ClientProjectMessage as ClientProjectMessage
import Wizard.Api.Models.WebSockets.ServerProjectMessage as ServerProjectMessage
import Wizard.Api.Projects as ProjectsApi
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Components.SummaryReport as SummaryReport
import Wizard.Data.AppState exposing (AppState)
import Wizard.Data.Session as Session
import Wizard.Msgs
import Wizard.Pages.Projects.Common.ProjectShareForm as QuestionnaireShareForm
import Wizard.Pages.Projects.Detail.Components.NewDocument as NewDocument
import Wizard.Pages.Projects.Detail.Components.Preview as Preview exposing (PreviewState(..))
import Wizard.Pages.Projects.Detail.Components.ProjectSaving as ProjectSaving
import Wizard.Pages.Projects.Detail.Components.ProjectVersionViewModal as ProjectVersionViewModal
import Wizard.Pages.Projects.Detail.Components.RevertModal as RevertModal
import Wizard.Pages.Projects.Detail.Components.Settings as Settings
import Wizard.Pages.Projects.Detail.Components.ShareModal as ShareModal
import Wizard.Pages.Projects.Detail.Documents.Update as Documents
import Wizard.Pages.Projects.Detail.Files.Update as Files
import Wizard.Pages.Projects.Detail.Models exposing (Model, addQuestionnaireEvent, addSavingActionUuid, removeSavingActionUuid)
import Wizard.Pages.Projects.Detail.Msgs exposing (Msg(..))
import Wizard.Pages.Projects.Detail.ProjectDetailRoute as ProjectDetailRoute
import Wizard.Pages.Projects.Routes exposing (Route(..))
import Wizard.Routes as Routes exposing (Route(..))
import Wizard.Routing as Routing exposing (cmdNavigate)
import Wizard.Utils.Driver as Driver
import Wizard.Utils.TourId as TourId


fetchData : AppState -> Uuid -> Model -> Cmd Msg
fetchData appState uuid model =
    let
        tourCmd =
            Driver.init (tour appState)
    in
    if ActionResult.unwrap False (.uuid >> (==) uuid) model.questionnaireCommon then
        Cmd.batch
            [ fetchSubrouteData appState model
            , WebSocket.open model.websocket
            , tourCmd
            ]

    else
        Cmd.batch
            [ fetchSubrouteData appState model
            , tourCmd
            ]


fetchSubrouteData : AppState -> Model -> Cmd Msg
fetchSubrouteData appState model =
    case appState.route of
        ProjectsRoute (DetailRoute uuid route) ->
            case route of
                ProjectDetailRoute.Questionnaire _ _ ->
                    Cmd.batch
                        [ Task.dispatch (QuestionnaireMsg Questionnaire.UpdateContentScroll)
                        , ProjectsApi.getQuestionnaire appState uuid GetQuestionnaireDetailCompleted
                        ]

                ProjectDetailRoute.Preview ->
                    ProjectsApi.getPreview appState uuid GetQuestionnairePreviewCompleted

                ProjectDetailRoute.Metrics ->
                    ProjectsApi.getSummaryReport appState uuid GetQuestionnaireSummaryReportCompleted

                ProjectDetailRoute.Documents _ ->
                    let
                        commonCmd =
                            if ActionResult.isSuccess model.questionnaireCommon then
                                Cmd.map DocumentsMsg Documents.fetchData

                            else
                                ProjectsApi.get appState uuid GetQuestionnaireCommonCompleted
                    in
                    Cmd.batch
                        [ commonCmd
                        , Cmd.map DocumentsMsg Documents.fetchData
                        ]

                ProjectDetailRoute.NewDocument _ ->
                    ProjectsApi.getSettings appState uuid GetQuestionnaireSettingsCompleted

                ProjectDetailRoute.Files _ ->
                    let
                        commonCmd =
                            if ActionResult.isSuccess model.questionnaireCommon then
                                Cmd.map FilesMsg Files.fetchData

                            else
                                ProjectsApi.get appState uuid GetQuestionnaireCommonCompleted
                    in
                    Cmd.batch
                        [ commonCmd
                        , Cmd.map FilesMsg Files.fetchData
                        ]

                ProjectDetailRoute.Settings ->
                    ProjectsApi.getSettings appState uuid GetQuestionnaireSettingsCompleted

                ProjectDetailRoute.Plugin _ ->
                    ProjectsApi.get appState uuid GetQuestionnaireCommonCompleted

        _ ->
            Cmd.none


tour : AppState -> TourConfig
tour appState =
    Driver.fromAppState TourId.projectsDetail appState
        |> Driver.addStep
            { element = selectDataTour "questionnaire_body"
            , popover =
                { title = gettext "Questionnaire" appState.locale
                , description = gettext "Fill out the questionnaire to provide key information for your data management plan. Changes are saved automatically." appState.locale
                }
            }
        |> Driver.addStep
            { element = selectDataTour "project_detail_share"
            , popover =
                { title = gettext "Collaboration" appState.locale
                , description = gettext "Share your project to collaborate with colleagues." appState.locale
                }
            }
        |> Driver.addStep
            { element = selectDataTour "navigation"
            , popover =
                { title = gettext "Navigation" appState.locale
                , description = gettext "Use the navigation bar to switch between tools and views related to your project." appState.locale
                }
            }


isGuarded : AppState -> Routes.Route -> Model -> Maybe String
isGuarded appState nextRoute model =
    if List.isEmpty model.savingActionUuids then
        Nothing

    else if Routes.isProjectsDetail model.uuid nextRoute then
        Nothing

    else
        Just (gettext "Some changes are still saving." appState.locale)


onUnload : Routes.Route -> Model -> Cmd Msg
onUnload nextRoute model =
    let
        leaveCmd =
            Cmd.batch
                [ WebSocket.close model.websocket
                , Task.dispatch ResetModel
                ]
    in
    case nextRoute of
        ProjectsRoute (DetailRoute uuid _) ->
            if uuid == model.uuid then
                Cmd.none

            else
                leaveCmd

        _ ->
            leaveCmd


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    let
        withSeed ( m, c ) =
            ( appState.seed, m, c )

        openWebSocket =
            if ActionResult.isLoading model.questionnaireCommon then
                WebSocket.open model.websocket

            else
                Cmd.none

        setError result error =
            let
                questionnaireRoute =
                    Routing.toUrl (Routes.projectsDetail model.uuid)

                loginRoute =
                    Routes.publicLogin (Just questionnaireRoute)
            in
            case ( error, Session.exists appState.session ) of
                ( BadStatus 403 _, False ) ->
                    withSeed ( model, cmdNavigate appState loginRoute )

                ( BadStatus 401 _, False ) ->
                    withSeed ( model, cmdNavigate appState loginRoute )

                ( BadStatus 401 _, True ) ->
                    withSeed ( model, Task.dispatch (Wizard.Msgs.logoutToMsg loginRoute) )

                _ ->
                    withSeed <|
                        ( { model | questionnaireCommon = ApiError.toActionResult appState (gettext "Unable to get the project." appState.locale) error }
                        , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
                        )
    in
    case msg of
        ResetModel ->
            withSeed ( { model | uuid = Uuid.nil }, Cmd.none )

        QuestionnaireMsg questionnaireMsg ->
            let
                ( questionnaireSeed, newQuestionnaireModel, questionnaireCmd ) =
                    case model.questionnaireModel of
                        Success questionnaireModel ->
                            Triple.mapSnd Success <|
                                Questionnaire.update
                                    questionnaireMsg
                                    (wrapMsg << QuestionnaireMsg)
                                    (Just Wizard.Msgs.SetFullscreen)
                                    appState
                                    { events = []
                                    , kmEditorUuid = Nothing
                                    }
                                    questionnaireModel

                        _ ->
                            ( appState.seed, model.questionnaireModel, Cmd.none )

                newModel1 =
                    { model | questionnaireModel = newQuestionnaireModel }

                applyAction seed updateQuestionnaire buildEvent =
                    let
                        ( uuid, applyActionSeed ) =
                            Random.step Uuid.uuidGenerator seed

                        event =
                            buildEvent uuid

                        applyActionModel =
                            newModel1
                                |> addSavingActionUuid uuid
                                |> addQuestionnaireEvent event

                        updatedQuestionnaireModel =
                            ActionResult.map updateQuestionnaire applyActionModel.questionnaireModel

                        updatedModel =
                            { applyActionModel | questionnaireModel = updatedQuestionnaireModel }

                        wsCmd =
                            event
                                |> ClientProjectMessage.SetContent
                                |> ClientProjectMessage.encode
                                |> WebSocket.send model.websocket

                        setUnloadMessageCmd =
                            Window.setUnloadMessage (gettext "Some changes are still saving." appState.locale)
                    in
                    ( applyActionSeed, updatedModel, Cmd.batch [ wsCmd, setUnloadMessageCmd ] )

                applyActionDebounce seed buildEvent =
                    let
                        ( uuid, applyActionSeed ) =
                            Random.step Uuid.uuidGenerator seed

                        event =
                            buildEvent uuid

                        path =
                            Maybe.withDefault "" (ProjectEvent.getPath event)

                        ( debounce, debounceCmd ) =
                            Debounce.push (debounceConfig appState path)
                                event
                                (getDebounceModel path newModel1)

                        updatedModel =
                            { newModel1 | questionnaireWebSocketDebounce = Dict.insert path debounce model.questionnaireWebSocketDebounce }

                        setUnloadMessageCmd =
                            Window.setUnloadMessage (gettext "Some changes are still saving." appState.locale)
                    in
                    ( applyActionSeed, updatedModel, Cmd.batch [ Cmd.map wrapMsg debounceCmd, setUnloadMessageCmd ] )

                createdAt =
                    appState.currentTime

                createdBy =
                    Maybe.map UserConfig.toUserSuggestion appState.config.user

                ( newSeed, newModel, newCmd ) =
                    case questionnaireMsg of
                        Questionnaire.PhaseModalUpdate _ mbPhaseUuid ->
                            if Maybe.isJust mbPhaseUuid then
                                applyAction questionnaireSeed identity <|
                                    \uuid ->
                                        ProjectEvent.SetPhase
                                            { uuid = uuid
                                            , phaseUuid = mbPhaseUuid
                                            , createdAt = createdAt
                                            , createdBy = createdBy
                                            }

                            else
                                ( appState.seed, newModel1, Cmd.none )

                        Questionnaire.SetReply path reply ->
                            applyActionDebounce questionnaireSeed <|
                                \uuid ->
                                    ProjectEvent.SetReply
                                        { uuid = uuid
                                        , path = path
                                        , value = reply.value
                                        , createdAt = createdAt
                                        , createdBy = createdBy
                                        }

                        Questionnaire.ClearReply path ->
                            applyAction questionnaireSeed identity <|
                                \uuid ->
                                    ProjectEvent.ClearReply
                                        { uuid = uuid
                                        , path = path
                                        , createdAt = createdAt
                                        , createdBy = createdBy
                                        }

                        Questionnaire.SetLabels path value ->
                            applyAction questionnaireSeed identity <|
                                \uuid ->
                                    ProjectEvent.SetLabels
                                        { uuid = uuid
                                        , path = path
                                        , value = value
                                        , createdAt = createdAt
                                        , createdBy = createdBy
                                        }

                        Questionnaire.CommentSubmit path mbThreadUuid text private ->
                            let
                                ( newThreadUuid, threadSeed ) =
                                    Random.step Uuid.uuidGenerator questionnaireSeed

                                ( commentUuid, commentSeed ) =
                                    Random.step Uuid.uuidGenerator threadSeed

                                threadUuid =
                                    Maybe.withDefault newThreadUuid mbThreadUuid

                                newThread =
                                    Maybe.isNothing mbThreadUuid

                                comment =
                                    { uuid = commentUuid
                                    , text = text
                                    , createdBy = createdBy
                                    , createdAt = createdAt
                                    , updatedAt = createdAt
                                    }

                                addComment questionnaire =
                                    Questionnaire.addComment path threadUuid private comment questionnaire
                            in
                            applyAction commentSeed addComment <|
                                \uuid ->
                                    ProjectEvent.AddComment
                                        { uuid = uuid
                                        , path = path
                                        , threadUuid = threadUuid
                                        , newThread = newThread
                                        , commentUuid = commentUuid
                                        , text = text
                                        , private = private
                                        , createdAt = createdAt
                                        , createdBy = createdBy
                                        }

                        Questionnaire.CommentDeleteSubmit path threadUuid commentUuid private ->
                            let
                                deleteComment questionnaire =
                                    Questionnaire.deleteComment path threadUuid commentUuid questionnaire
                            in
                            applyAction questionnaireSeed deleteComment <|
                                \uuid ->
                                    ProjectEvent.DeleteComment
                                        { uuid = uuid
                                        , path = path
                                        , threadUuid = threadUuid
                                        , commentUuid = commentUuid
                                        , private = private
                                        , createdAt = createdAt
                                        , createdBy = createdBy
                                        }

                        Questionnaire.CommentEditSubmit path threadUuid commentUuid text private ->
                            let
                                editComment questionnaire =
                                    Questionnaire.editComment path threadUuid commentUuid appState.currentTime text questionnaire
                            in
                            applyAction questionnaireSeed editComment <|
                                \uuid ->
                                    ProjectEvent.EditComment
                                        { uuid = uuid
                                        , path = path
                                        , threadUuid = threadUuid
                                        , commentUuid = commentUuid
                                        , text = text
                                        , private = private
                                        , createdAt = createdAt
                                        , createdBy = createdBy
                                        }

                        Questionnaire.CommentThreadDelete path commentThread ->
                            let
                                deleteCommentThread questionnaire =
                                    Questionnaire.deleteCommentThread path commentThread.uuid questionnaire
                            in
                            applyAction questionnaireSeed deleteCommentThread <|
                                \uuid ->
                                    ProjectEvent.DeleteCommentThread
                                        { uuid = uuid
                                        , path = path
                                        , threadUuid = commentThread.uuid
                                        , private = commentThread.private
                                        , createdAt = createdAt
                                        , createdBy = createdBy
                                        }

                        Questionnaire.CommentThreadResolve path commentThread ->
                            let
                                resolveCommentThread questionnaire =
                                    Questionnaire.resolveCommentThread path commentThread.uuid (CommentThread.commentCount commentThread) questionnaire
                            in
                            applyAction questionnaireSeed resolveCommentThread <|
                                \uuid ->
                                    ProjectEvent.ResolveCommentThread
                                        { uuid = uuid
                                        , path = path
                                        , threadUuid = commentThread.uuid
                                        , private = commentThread.private
                                        , createdAt = createdAt
                                        , createdBy = createdBy
                                        , commentCount = List.length commentThread.comments
                                        }

                        Questionnaire.CommentThreadReopen path commentThread ->
                            let
                                reopenCommentThread questionnaire =
                                    Questionnaire.reopenCommentThread path commentThread.uuid (CommentThread.commentCount commentThread) questionnaire
                            in
                            applyAction questionnaireSeed reopenCommentThread <|
                                \uuid ->
                                    ProjectEvent.ReopenCommentThread
                                        { uuid = uuid
                                        , path = path
                                        , threadUuid = commentThread.uuid
                                        , private = commentThread.private
                                        , createdAt = createdAt
                                        , createdBy = createdBy
                                        , commentCount = List.length commentThread.comments
                                        }

                        Questionnaire.CommentThreadAssign path commentThread mbUser ->
                            let
                                assignCommentThread questionnaire =
                                    Questionnaire.assignCommentThread path commentThread.uuid mbUser questionnaire
                            in
                            applyAction questionnaireSeed assignCommentThread <|
                                \uuid ->
                                    ProjectEvent.AssignCommentThread
                                        { uuid = uuid
                                        , path = path
                                        , threadUuid = commentThread.uuid
                                        , private = commentThread.private
                                        , assignedTo = mbUser
                                        , createdAt = createdAt
                                        , createdBy = createdBy
                                        }

                        _ ->
                            ( appState.seed, newModel1, Cmd.none )
            in
            ( newSeed
            , newModel
            , Cmd.batch [ questionnaireCmd, newCmd ]
            )

        QuestionnaireDebounceMsg path debounceMsg ->
            let
                send event =
                    let
                        wsCmd =
                            event
                                |> ClientProjectMessage.SetContent
                                |> ClientProjectMessage.encode
                                |> WebSocket.send model.websocket
                    in
                    Cmd.batch
                        [ wsCmd
                        , Task.dispatch (QuestionnaireAddSavingEvent event)
                        ]

                ( debounce, cmd ) =
                    Debounce.update
                        (debounceConfig appState path)
                        (Debounce.takeLast send)
                        debounceMsg
                        (getDebounceModel path model)
            in
            withSeed <|
                ( { model | questionnaireWebSocketDebounce = Dict.insert path debounce model.questionnaireWebSocketDebounce }
                , Cmd.map wrapMsg cmd
                )

        QuestionnaireAddSavingEvent questionnaireEvent ->
            let
                newModel =
                    model
                        |> addSavingActionUuid (ProjectEvent.getUuid questionnaireEvent)
                        |> addQuestionnaireEvent questionnaireEvent
            in
            withSeed ( newModel, Cmd.none )

        PreviewMsg previewMsg ->
            let
                ( previewModel, previewCmd ) =
                    Preview.update previewMsg appState model.previewModel
            in
            withSeed <|
                ( { model | previewModel = previewModel }
                , Cmd.map (wrapMsg << PreviewMsg) previewCmd
                )

        SummaryReportMsg summaryReportMsg ->
            withSeed <|
                ( model
                , SummaryReport.update summaryReportMsg
                )

        DocumentsMsg documentsMsg ->
            let
                ( documentsModel, documentsCmd ) =
                    Documents.update (wrapMsg << DocumentsMsg) documentsMsg appState model.uuid model.documentsModel
            in
            withSeed <|
                ( { model | documentsModel = documentsModel }
                , documentsCmd
                )

        NewDocumentMsg newDocumentMsg ->
            case model.questionnaireCommon of
                Success questionnaire ->
                    let
                        ( newDocumentModel, newDocumentCmd ) =
                            NewDocument.update
                                { wrapMsg = wrapMsg << NewDocumentMsg
                                , projectUuid = questionnaire.uuid
                                , knowledgeModelPackageId = questionnaire.knowledgeModelPackageId
                                , documentsNavigateCmd = cmdNavigate appState <| Routes.projectsDetailDocuments questionnaire.uuid
                                }
                                newDocumentMsg
                                appState
                                model.newDocumentModel
                    in
                    withSeed <|
                        ( { model | newDocumentModel = newDocumentModel }
                        , newDocumentCmd
                        )

                _ ->
                    withSeed ( model, Cmd.none )

        FilesMsg filesMsg ->
            let
                ( filesModel, filesCmd ) =
                    Files.update filesMsg (wrapMsg << FilesMsg) appState model.uuid model.filesModel
            in
            withSeed <|
                ( { model | filesModel = filesModel }
                , filesCmd
                )

        GetQuestionnaireCommonCompleted result ->
            case result of
                Ok data ->
                    let
                        newModel =
                            { model | questionnaireCommon = Success data }
                    in
                    withSeed ( newModel, openWebSocket )

                Err error ->
                    setError result error

        GetQuestionnaireDetailCompleted result ->
            case result of
                Ok data ->
                    let
                        ( questionnaireModel, questionnaireCmd ) =
                            Questionnaire.init appState data.data model.mbSelectedPath model.mbCommentThreadUuid

                        newModel =
                            { model
                                | questionnaireModel = Success questionnaireModel
                                , questionnaireCommon = Success data.common
                            }
                    in
                    withSeed <|
                        ( newModel
                        , Cmd.batch
                            [ openWebSocket
                            , Cmd.map (wrapMsg << QuestionnaireMsg) questionnaireCmd
                            ]
                        )

                Err error ->
                    setError result error

        GetQuestionnaireSummaryReportCompleted result ->
            case result of
                Ok data ->
                    let
                        newModel =
                            { model
                                | questionnaireSummaryReport = Success data.data
                                , questionnaireCommon = Success data.common
                            }
                    in
                    withSeed ( newModel, openWebSocket )

                Err error ->
                    setError result error

        GetQuestionnairePreviewCompleted result ->
            case result of
                Ok data ->
                    let
                        hasTemplate =
                            ProjectPreview.hasTemplateSet data.data

                        templateState =
                            if hasTemplate then
                                Preview Loading

                            else
                                TemplateNotSet

                        newModel =
                            { model
                                | questionnairePreview = Success data.data
                                , questionnaireCommon = Success data.common
                                , previewModel = Preview.init model.uuid templateState
                            }
                    in
                    withSeed
                        ( newModel
                        , Cmd.batch
                            [ openWebSocket
                            , Cmd.map (wrapMsg << PreviewMsg) (Preview.fetchData appState model.uuid hasTemplate)
                            ]
                        )

                Err error ->
                    setError result error

        GetQuestionnaireSettingsCompleted result ->
            case result of
                Ok data ->
                    let
                        newModel =
                            { model
                                | questionnaireSettings = Success data.data
                                , settingsModel = Settings.init appState (Just data.data)
                                , questionnaireCommon = Success data.common
                            }

                        ( newModelWithDocument, newDocumentCmd ) =
                            case appState.route of
                                ProjectsRoute (DetailRoute uuid (ProjectDetailRoute.NewDocument mbEventUuid)) ->
                                    ( { newModel | newDocumentModel = NewDocument.initialModel data.data mbEventUuid }
                                    , Cmd.map (wrapMsg << NewDocumentMsg) <|
                                        NewDocument.fetchData appState uuid mbEventUuid
                                    )

                                _ ->
                                    ( newModel, Cmd.none )
                    in
                    withSeed ( newModelWithDocument, Cmd.batch [ openWebSocket, newDocumentCmd ] )

                Err error ->
                    setError result error

        WebSocketMsg wsMsg ->
            handleWebsocketMsg wsMsg appState model

        WebSocketPing ->
            withSeed ( model, WebSocket.ping model.websocket )

        ProjectSavingMsg qsMsg ->
            withSeed ( { model | projectSavingModel = ProjectSaving.update qsMsg model.projectSavingModel }, Cmd.none )

        ShareModalMsg shareModalMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << ShareModalMsg
                    , projectUuid = model.uuid
                    , permissions = ActionResult.unwrap [] (.questionnaire >> .permissions) model.questionnaireModel
                    , onCloseMsg = wrapMsg ShareModalCloseMsg
                    }

                ( newSeed, shareModalModel, cmd ) =
                    ShareModal.update updateConfig shareModalMsg appState model.shareModalModel
            in
            ( newSeed, { model | shareModalModel = shareModalModel }, cmd )

        ShareModalCloseMsg ->
            withSeed ( { model | questionnaireModel = ActionResult.map Questionnaire.resetUserSuggestionDropdownModels model.questionnaireModel }, Cmd.none )

        ShareDropdownMsg dropdownState ->
            withSeed ( { model | shareDropdownState = dropdownState }, Cmd.none )

        ShareDropdownCopyLink ->
            let
                link =
                    appState.clientUrl ++ String.replace "/wizard" "" (Routing.toUrl (Routes.projectsDetail model.uuid))
            in
            withSeed ( model, Ports.copyToClipboard link )

        SettingsMsg settingsMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << SettingsMsg
                    , redirectCmd = cmdNavigate appState (Routes.projectsIndex appState)
                    , knowledgeModelPackageId = ActionResult.unwrap "" .knowledgeModelPackageId model.questionnaireCommon
                    , projectUuid = model.uuid
                    , permissions = ActionResult.unwrap [] .permissions model.questionnaireCommon
                    }

                ( settingsModel, cmd ) =
                    Settings.update updateConfig settingsMsg appState model.settingsModel
            in
            withSeed ( { model | settingsModel = settingsModel }, cmd )

        Refresh ->
            withSeed ( model, Window.refresh () )

        QuestionnaireVersionViewModalMsg qMsg ->
            let
                ( newQuestionnaireVersionViewModalModel, cmd ) =
                    ProjectVersionViewModal.update appState qMsg model.questionnaireVersionViewModalModel
            in
            withSeed
                ( { model | questionnaireVersionViewModalModel = newQuestionnaireVersionViewModalModel }
                , Cmd.map (wrapMsg << QuestionnaireVersionViewModalMsg) cmd
                )

        OpenVersionPreview projectUuid eventUuid ->
            let
                ( newQuestionnaireVersionViewModalModel, cmd ) =
                    ProjectVersionViewModal.init appState projectUuid eventUuid
            in
            withSeed
                ( { model | questionnaireVersionViewModalModel = newQuestionnaireVersionViewModalModel }
                , Cmd.map (wrapMsg << QuestionnaireVersionViewModalMsg) cmd
                )

        RevertModalMsg rMsg ->
            case model.questionnaireModel of
                Success questionnaireModel ->
                    let
                        cfg =
                            { projectUuid = questionnaireModel.questionnaire.uuid }

                        ( newRevertModalModel, cmd ) =
                            RevertModal.update cfg appState rMsg model.revertModalModel
                    in
                    withSeed
                        ( { model | revertModalModel = newRevertModalModel }
                        , Cmd.map (wrapMsg << RevertModalMsg) cmd
                        )

                _ ->
                    ( appState.seed, model, Cmd.none )

        OpenRevertModal event ->
            let
                newRevertModalModel =
                    RevertModal.setEvent event model.revertModalModel
            in
            withSeed ( { model | revertModalModel = newRevertModalModel }, Cmd.none )

        AddToMyProjects ->
            case model.questionnaireCommon of
                Success questionnaire ->
                    let
                        member =
                            Member.userMember
                                { uuid = Maybe.unwrap Uuid.nil .uuid appState.config.user
                                , firstName = ""
                                , lastName = ""
                                , gravatarHash = ""
                                , imageUrl = Nothing
                                }

                        permission =
                            { member = member
                            , perms = ProjectPerm.all
                            }

                        questionnaireCommon =
                            { questionnaire | permissions = [ permission ] }

                        questionnaireEditForm =
                            QuestionnaireShareForm.init questionnaireCommon

                        cmd =
                            case Form.getOutput questionnaireEditForm of
                                Just form ->
                                    Cmd.map wrapMsg <|
                                        ProjectsApi.putShare appState
                                            model.uuid
                                            (QuestionnaireShareForm.encode form)
                                            PutQuestionnaireComplete

                                _ ->
                                    Cmd.none
                    in
                    withSeed ( { model | addingToMyProjects = Loading }, cmd )

                _ ->
                    ( appState.seed, model, Cmd.none )

        PutQuestionnaireComplete result ->
            case result of
                Ok _ ->
                    ( appState.seed, model, Window.refresh () )

                Err error ->
                    ( appState.seed, { model | addingToMyProjects = ApiError.toActionResult appState (gettext "Questionnaire could not be saved." appState.locale) error }, Cmd.none )


handleWebsocketMsg : WebSocket.RawMsg -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
handleWebsocketMsg websocketMsg appState model =
    let
        updateQuestionnaire event actionUuid fn =
            let
                ( newModel, removed ) =
                    removeSavingActionUuid actionUuid model

                newModel2 =
                    if not removed then
                        addQuestionnaireEvent event <|
                            { newModel | questionnaireModel = ActionResult.map fn newModel.questionnaireModel }

                    else
                        newModel

                clearUnloadMessageCmd =
                    if removed && List.isEmpty newModel2.savingActionUuids then
                        Window.clearUnloadMessage ()

                    else
                        Cmd.none
            in
            ( appState.seed
            , newModel2
            , clearUnloadMessageCmd
            )

        updateQuestionnaireData data =
            ( appState.seed
            , { model
                | questionnaireCommon = ActionResult.map (ProjectCommon.updateWithQuestionnaireData data) model.questionnaireCommon
                , questionnaireModel = ActionResult.map (Questionnaire.updateWithQuestionnaireData appState data) model.questionnaireModel
              }
            , Cmd.none
            )
    in
    case WebSocket.receive (WebSocketServerAction.decoder ServerProjectMessage.decoder) websocketMsg model.websocket of
        WebSocket.Message serverAction ->
            case serverAction of
                WebSocketServerAction.Success message ->
                    case message of
                        ServerProjectMessage.SetUserList users ->
                            ( appState.seed, { model | onlineUsers = users }, Cmd.none )

                        ServerProjectMessage.SetContent event ->
                            case event of
                                ProjectEvent.SetReply data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.setReply data.path (SetReplyData.toReply data))

                                ProjectEvent.ClearReply data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.clearReply data.path)

                                ProjectEvent.SetPhase data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.setPhaseUuid data.phaseUuid)

                                ProjectEvent.SetLabels data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.setLabels data.path data.value)

                                ProjectEvent.ResolveCommentThread data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.resolveCommentThread data.path data.threadUuid data.commentCount)

                                ProjectEvent.ReopenCommentThread data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.reopenCommentThread data.path data.threadUuid data.commentCount)

                                ProjectEvent.DeleteCommentThread data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.deleteCommentThread data.path data.threadUuid)

                                ProjectEvent.AssignCommentThread data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.assignCommentThread data.path data.threadUuid data.assignedTo)

                                ProjectEvent.AddComment data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.addComment data.path data.threadUuid data.private (AddCommentData.toComment data))

                                ProjectEvent.EditComment data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.editComment data.path data.threadUuid data.commentUuid data.createdAt data.text)

                                ProjectEvent.DeleteComment data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.deleteComment data.path data.threadUuid data.commentUuid)

                        ServerProjectMessage.SetProject data ->
                            updateQuestionnaireData data

                        ServerProjectMessage.AddFile file ->
                            ( appState.seed
                            , { model | questionnaireModel = ActionResult.map (Questionnaire.addFile file) model.questionnaireModel }
                            , Cmd.none
                            )

                WebSocketServerAction.Error code ->
                    if code == "error.service.qtn.collaboration.force_disconnect" then
                        ( appState.seed, { model | forceDisconnect = True }, Cmd.none )

                    else
                        ( appState.seed, { model | error = True }, Cmd.none )

        WebSocket.Close ->
            ( appState.seed, { model | offline = True }, Cmd.none )

        _ ->
            ( appState.seed, model, Cmd.none )


debounceConfig : AppState -> String -> Debounce.Config Msg
debounceConfig appState path =
    { strategy = Debounce.soon appState.websocketThrottleDelay
    , transform = QuestionnaireDebounceMsg path
    }


getDebounceModel : String -> Model -> Debounce.Debounce ProjectEvent
getDebounceModel path model =
    Maybe.withDefault Debounce.init (Dict.get path model.questionnaireWebSocketDebounce)
