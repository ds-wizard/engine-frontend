module Wizard.Projects.Detail.Update exposing (fetchData, isGuarded, onUnload, update)

import ActionResult exposing (ActionResult(..))
import Debounce
import Dict
import Form
import Gettext exposing (gettext)
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Auth.Session as Session
import Shared.Copy as Ports
import Shared.Data.Member as Member
import Shared.Data.QuestionnaireCommon as QuestionnaireCommon
import Shared.Data.QuestionnaireDetail.CommentThread as CommentThread
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.AddCommentData as AddCommentData
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetReplyData as SetReplyData
import Shared.Data.QuestionnairePerm as QuestionnairePerm
import Shared.Data.QuestionnairePreview as QuestionnairePreview
import Shared.Data.UserInfo as UserInfo
import Shared.Data.WebSockets.ClientQuestionnaireAction as ClientQuestionnaireAction
import Shared.Data.WebSockets.ServerQuestionnaireAction as ServerQuestionnaireAction
import Shared.Data.WebSockets.WebSocketServerAction as WebSocketServerAction
import Shared.Error.ApiError as ApiError exposing (ApiError(..))
import Shared.Utils exposing (dispatch)
import Shared.WebSocket as WebSocket
import Triple
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.SummaryReport as SummaryReport
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Projects.Common.QuestionnaireShareForm as QuestionnaireShareForm
import Wizard.Projects.Detail.Components.NewDocument as NewDocument
import Wizard.Projects.Detail.Components.Preview as Preview exposing (PreviewState(..))
import Wizard.Projects.Detail.Components.ProjectSaving as ProjectSaving
import Wizard.Projects.Detail.Components.QuestionnaireVersionViewModal as QuestionnaireVersionViewModal
import Wizard.Projects.Detail.Components.RevertModal as RevertModal
import Wizard.Projects.Detail.Components.Settings as Settings
import Wizard.Projects.Detail.Components.ShareModal as ShareModal
import Wizard.Projects.Detail.Documents.Update as Documents
import Wizard.Projects.Detail.Files.Update as Files
import Wizard.Projects.Detail.Models exposing (Model, addQuestionnaireEvent, addSavingActionUuid, removeSavingActionUuid)
import Wizard.Projects.Detail.Msgs exposing (Msg(..))
import Wizard.Projects.Detail.ProjectDetailRoute as ProjectDetailRoute
import Wizard.Projects.Routes exposing (Route(..))
import Wizard.Routes as Routes exposing (Route(..))
import Wizard.Routing as Routing exposing (cmdNavigate)


fetchData : AppState -> Uuid -> Model -> Cmd Msg
fetchData appState uuid model =
    if ActionResult.unwrap False (.uuid >> (==) uuid) model.questionnaireCommon then
        Cmd.batch
            [ fetchSubrouteData appState model
            , WebSocket.open model.websocket
            ]

    else
        fetchSubrouteData appState model


fetchSubrouteData : AppState -> Model -> Cmd Msg
fetchSubrouteData appState model =
    case appState.route of
        ProjectsRoute (DetailRoute uuid route) ->
            case route of
                ProjectDetailRoute.Questionnaire _ _ ->
                    Cmd.batch
                        [ dispatch (QuestionnaireMsg Questionnaire.UpdateContentScroll)
                        , QuestionnairesApi.getQuestionnaireQuestionnaire uuid appState GetQuestionnaireDetailCompleted
                        ]

                ProjectDetailRoute.Preview ->
                    QuestionnairesApi.getQuestionnairePreview uuid appState GetQuestionnairePreviewCompleted

                ProjectDetailRoute.Metrics ->
                    QuestionnairesApi.getSummaryReport uuid appState GetQuestionnaireSummaryReportCompleted

                ProjectDetailRoute.Documents _ ->
                    let
                        commonCmd =
                            if ActionResult.isSuccess model.questionnaireCommon then
                                Cmd.map DocumentsMsg Documents.fetchData

                            else
                                QuestionnairesApi.getQuestionnaire uuid appState GetQuestionnaireCommonCompleted
                    in
                    Cmd.batch
                        [ commonCmd
                        , Cmd.map DocumentsMsg Documents.fetchData
                        ]

                ProjectDetailRoute.NewDocument _ ->
                    QuestionnairesApi.getQuestionnaireSettings uuid appState GetQuestionnaireSettingsCompleted

                ProjectDetailRoute.Files _ ->
                    let
                        commonCmd =
                            if ActionResult.isSuccess model.questionnaireCommon then
                                Cmd.map FilesMsg Files.fetchData

                            else
                                QuestionnairesApi.getQuestionnaire uuid appState GetQuestionnaireCommonCompleted
                    in
                    Cmd.batch
                        [ commonCmd
                        , Cmd.map FilesMsg Files.fetchData
                        ]

                ProjectDetailRoute.Settings ->
                    QuestionnairesApi.getQuestionnaireSettings uuid appState GetQuestionnaireSettingsCompleted

        _ ->
            Cmd.none


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
                , dispatch ResetModel
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
                    Routing.toUrl appState (Routes.projectsDetail model.uuid)

                loginRoute =
                    Routes.publicLogin (Just questionnaireRoute)
            in
            case ( error, Session.exists appState.session ) of
                ( BadStatus 403 _, False ) ->
                    withSeed ( model, cmdNavigate appState loginRoute )

                ( BadStatus 401 _, False ) ->
                    withSeed ( model, cmdNavigate appState loginRoute )

                ( BadStatus 401 _, True ) ->
                    withSeed ( model, dispatch (Wizard.Msgs.logoutToMsg loginRoute) )

                _ ->
                    withSeed <|
                        ( { model | questionnaireCommon = ApiError.toActionResult appState (gettext "Unable to get the project." appState.locale) error }
                        , getResultCmd Wizard.Msgs.logoutMsg result
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
                                    { events = [] }
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
                                |> ClientQuestionnaireAction.SetContent
                                |> ClientQuestionnaireAction.encode
                                |> WebSocket.send model.websocket

                        setUnloadMessageCmd =
                            Ports.setUnloadMessage (gettext "Some changes are still saving." appState.locale)
                    in
                    ( applyActionSeed, updatedModel, Cmd.batch [ wsCmd, setUnloadMessageCmd ] )

                applyActionDebounce seed buildEvent =
                    let
                        ( uuid, applyActionSeed ) =
                            Random.step Uuid.uuidGenerator seed

                        event =
                            buildEvent uuid

                        path =
                            Maybe.withDefault "" (QuestionnaireEvent.getPath event)

                        ( debounce, debounceCmd ) =
                            Debounce.push (debounceConfig appState path)
                                event
                                (getDebounceModel path newModel1)

                        updatedModel =
                            { newModel1 | questionnaireWebSocketDebounce = Dict.insert path debounce model.questionnaireWebSocketDebounce }

                        setUnloadMessageCmd =
                            Ports.setUnloadMessage (gettext "Some changes are still saving." appState.locale)
                    in
                    ( applyActionSeed, updatedModel, Cmd.batch [ Cmd.map wrapMsg debounceCmd, setUnloadMessageCmd ] )

                createdAt =
                    appState.currentTime

                createdBy =
                    Maybe.map UserInfo.toUserSuggestion appState.config.user

                ( newSeed, newModel, newCmd ) =
                    case questionnaireMsg of
                        Questionnaire.PhaseModalUpdate _ mbPhaseUuid ->
                            if Maybe.isJust mbPhaseUuid then
                                applyAction questionnaireSeed identity <|
                                    \uuid ->
                                        QuestionnaireEvent.SetPhase
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
                                    QuestionnaireEvent.SetReply
                                        { uuid = uuid
                                        , path = path
                                        , value = reply.value
                                        , createdAt = createdAt
                                        , createdBy = createdBy
                                        }

                        Questionnaire.ClearReply path ->
                            applyAction questionnaireSeed identity <|
                                \uuid ->
                                    QuestionnaireEvent.ClearReply
                                        { uuid = uuid
                                        , path = path
                                        , createdAt = createdAt
                                        , createdBy = createdBy
                                        }

                        Questionnaire.SetLabels path value ->
                            applyAction questionnaireSeed identity <|
                                \uuid ->
                                    QuestionnaireEvent.SetLabels
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
                                    QuestionnaireEvent.AddComment
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
                                    QuestionnaireEvent.DeleteComment
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
                                    QuestionnaireEvent.EditComment
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
                                    QuestionnaireEvent.DeleteCommentThread
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
                                    QuestionnaireEvent.ResolveCommentThread
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
                                    QuestionnaireEvent.ReopenCommentThread
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
                                    QuestionnaireEvent.AssignCommentThread
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
                                |> ClientQuestionnaireAction.SetContent
                                |> ClientQuestionnaireAction.encode
                                |> WebSocket.send model.websocket
                    in
                    Cmd.batch
                        [ wsCmd
                        , dispatch (QuestionnaireAddSavingEvent event)
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
                        |> addSavingActionUuid (QuestionnaireEvent.getUuid questionnaireEvent)
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
                                , questionnaireUuid = questionnaire.uuid
                                , packageId = questionnaire.packageId
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
                            QuestionnairePreview.hasTemplateSet data.data

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
                    , questionnaireUuid = model.uuid
                    , permissions = ActionResult.unwrap [] (.questionnaire >> .permissions) model.questionnaireModel
                    }

                ( newSeed, shareModalModel, cmd ) =
                    ShareModal.update updateConfig shareModalMsg appState model.shareModalModel
            in
            ( newSeed, { model | shareModalModel = shareModalModel }, cmd )

        ShareDropdownMsg dropdownState ->
            withSeed ( { model | shareDropdownState = dropdownState }, Cmd.none )

        ShareDropdownCopyLink ->
            let
                link =
                    appState.clientUrl ++ String.replace "/wizard" "" (Routing.toUrl appState (Routes.projectsDetail model.uuid))
            in
            withSeed ( model, Ports.copyToClipboard link )

        SettingsMsg settingsMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << SettingsMsg
                    , redirectCmd = cmdNavigate appState (Routes.projectsIndex appState)
                    , packageId = ActionResult.unwrap "" (.questionnaire >> .packageId) model.questionnaireModel
                    , questionnaireUuid = model.uuid
                    , permissions = ActionResult.unwrap [] (.questionnaire >> .permissions) model.questionnaireModel
                    }

                ( settingsModel, cmd ) =
                    Settings.update updateConfig settingsMsg appState model.settingsModel
            in
            withSeed ( { model | settingsModel = settingsModel }, cmd )

        Refresh ->
            withSeed ( model, Ports.refresh () )

        QuestionnaireVersionViewModalMsg qMsg ->
            case model.questionnaireModel of
                Success questionnaireModel ->
                    let
                        ( newQuestionnaireVersionViewModalModel, cmd ) =
                            QuestionnaireVersionViewModal.update qMsg questionnaireModel.questionnaire appState model.questionnaireVersionViewModalModel
                    in
                    withSeed
                        ( { model | questionnaireVersionViewModalModel = newQuestionnaireVersionViewModalModel }
                        , Cmd.map (wrapMsg << QuestionnaireVersionViewModalMsg) cmd
                        )

                _ ->
                    ( appState.seed, model, Cmd.none )

        OpenVersionPreview questionnaireUuid eventUuid ->
            let
                ( newQuestionnaireVersionViewModalModel, cmd ) =
                    QuestionnaireVersionViewModal.init appState questionnaireUuid eventUuid
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
                            { questionnaireUuid = questionnaireModel.questionnaire.uuid }

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
                            , perms = QuestionnairePerm.all
                            }

                        questionnaireCommon =
                            { questionnaire | permissions = [ permission ] }

                        questionnaireEditForm =
                            QuestionnaireShareForm.init questionnaireCommon

                        cmd =
                            case Form.getOutput questionnaireEditForm of
                                Just form ->
                                    Cmd.map wrapMsg <|
                                        QuestionnairesApi.putQuestionnaireShare model.uuid
                                            (QuestionnaireShareForm.encode form)
                                            appState
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
                    ( appState.seed, model, Ports.refresh () )

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
                        Ports.clearUnloadMessage ()

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
                | questionnaireCommon = ActionResult.map (QuestionnaireCommon.updateWithQuestionnaireData data) model.questionnaireCommon
                , questionnaireModel = ActionResult.map (Questionnaire.updateWithQuestionnaireData appState data) model.questionnaireModel
              }
            , Cmd.none
            )
    in
    case WebSocket.receive ServerQuestionnaireAction.decoder websocketMsg model.websocket of
        WebSocket.Message serverAction ->
            case serverAction of
                WebSocketServerAction.Success message ->
                    case message of
                        ServerQuestionnaireAction.SetUserList users ->
                            ( appState.seed, { model | onlineUsers = users }, Cmd.none )

                        ServerQuestionnaireAction.SetContent event ->
                            case event of
                                QuestionnaireEvent.SetReply data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.setReply data.path (SetReplyData.toReply data))

                                QuestionnaireEvent.ClearReply data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.clearReply data.path)

                                QuestionnaireEvent.SetPhase data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.setPhaseUuid data.phaseUuid)

                                QuestionnaireEvent.SetLabels data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.setLabels data.path data.value)

                                QuestionnaireEvent.ResolveCommentThread data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.resolveCommentThread data.path data.threadUuid data.commentCount)

                                QuestionnaireEvent.ReopenCommentThread data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.reopenCommentThread data.path data.threadUuid data.commentCount)

                                QuestionnaireEvent.DeleteCommentThread data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.deleteCommentThread data.path data.threadUuid)

                                QuestionnaireEvent.AssignCommentThread data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.assignCommentThread data.path data.threadUuid data.assignedTo)

                                QuestionnaireEvent.AddComment data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.addComment data.path data.threadUuid data.private (AddCommentData.toComment data))

                                QuestionnaireEvent.EditComment data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.editComment data.path data.threadUuid data.commentUuid data.createdAt data.text)

                                QuestionnaireEvent.DeleteComment data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.deleteComment data.path data.threadUuid data.commentUuid)

                        ServerQuestionnaireAction.SetQuestionnaire data ->
                            updateQuestionnaireData data

                        ServerQuestionnaireAction.AddFile file ->
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


getDebounceModel : String -> Model -> Debounce.Debounce QuestionnaireEvent
getDebounceModel path model =
    Maybe.withDefault Debounce.init (Dict.get path model.questionnaireWebSocketDebounce)
