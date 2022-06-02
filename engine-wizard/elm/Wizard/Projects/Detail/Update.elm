module Wizard.Projects.Detail.Update exposing (fetchData, isGuarded, onUnload, update)

import ActionResult exposing (ActionResult(..))
import Form
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Auth.Session as Session
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.AddCommentData as AddCommentData
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetReplyData as SetReplyData
import Shared.Data.UserInfo as UserInfo
import Shared.Data.WebSockets.ClientQuestionnaireAction as ClientQuestionnaireAction
import Shared.Data.WebSockets.ServerQuestionnaireAction as ServerQuestionnaireAction
import Shared.Data.WebSockets.WebSocketServerAction as WebSocketServerAction
import Shared.Error.ApiError as ApiError exposing (ApiError(..))
import Shared.Locale exposing (l, lg)
import Shared.Utils exposing (dispatch, getUuid)
import Shared.WebSocket as WebSocket
import Triple
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.OnlineUser as OnlineUser
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.SummaryReport as SummaryReport
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Projects.Common.QuestionnaireEditForm as QuestionnaireEditForm
import Wizard.Projects.Detail.Components.NewDocument as NewDocument
import Wizard.Projects.Detail.Components.Preview as Preview
import Wizard.Projects.Detail.Components.ProjectSaving as ProjectSaving
import Wizard.Projects.Detail.Components.QuestionnaireVersionViewModal as QuestionnaireVersionViewModal
import Wizard.Projects.Detail.Components.RevertModal as RevertModal
import Wizard.Projects.Detail.Components.Settings as Settings
import Wizard.Projects.Detail.Components.ShareModal as ShareModal
import Wizard.Projects.Detail.Documents.Update as Documents
import Wizard.Projects.Detail.Models exposing (Model, addQuestionnaireEvent, addSavingActionUuid, hasTemplate, initPageModel, removeSavingActionUuid)
import Wizard.Projects.Detail.Msgs exposing (Msg(..))
import Wizard.Projects.Detail.ProjectDetailRoute as PlanDetailRoute
import Wizard.Projects.Routes exposing (Route(..))
import Wizard.Routes as Routes exposing (Route(..))
import Wizard.Routing as Routing exposing (cmdNavigate)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Detail.Update"


fetchData : AppState -> Uuid -> Model -> Cmd Msg
fetchData appState uuid model =
    if ActionResult.unwrap False (.uuid >> (==) uuid) model.questionnaireModel then
        Cmd.batch
            [ fetchSubrouteData appState model
            , WebSocket.open model.websocket
            ]

    else
        QuestionnairesApi.getQuestionnaire uuid appState GetQuestionnaireComplete


fetchSubrouteData : AppState -> Model -> Cmd Msg
fetchSubrouteData appState model =
    case appState.route of
        ProjectsRoute (DetailRoute uuid route) ->
            case route of
                PlanDetailRoute.Preview ->
                    Cmd.map PreviewMsg <|
                        Preview.fetchData appState uuid (hasTemplate model)

                PlanDetailRoute.Metrics ->
                    Cmd.map SummaryReportMsg <|
                        SummaryReport.fetchData appState uuid

                PlanDetailRoute.Documents _ ->
                    Cmd.map DocumentsMsg <|
                        Documents.fetchData

                PlanDetailRoute.NewDocument _ ->
                    Cmd.map NewDocumentMsg <|
                        NewDocument.fetchData appState uuid

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


fetchSubrouteDataFromAfter : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
fetchSubrouteDataFromAfter wrapMsg appState model =
    case ( model.questionnaireModel, appState.route ) of
        ( Success _, ProjectsRoute (DetailRoute _ route) ) ->
            ( initPageModel appState route model, Cmd.map wrapMsg <| fetchSubrouteData appState model )

        _ ->
            ( model, Cmd.none )


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    if List.isEmpty model.savingActionUuids then
        Nothing

    else
        Just (l_ "unloadMessage" appState)


onUnload : Routes.Route -> Model -> Cmd Msg
onUnload newRoute model =
    let
        leaveCmd =
            Cmd.batch
                [ WebSocket.close model.websocket
                , dispatch ResetModel
                ]
    in
    case newRoute of
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
                            addQuestionnaireEvent event <|
                                addSavingActionUuid uuid newModel1

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
                            Ports.setUnloadMessage (l_ "unloadMessage" appState)
                    in
                    ( applyActionSeed, updatedModel, Cmd.batch [ wsCmd, setUnloadMessageCmd ] )

                createdAt =
                    appState.currentTime

                createdBy =
                    Maybe.map UserInfo.toUserSuggestion appState.session.user

                ( newSeed, newModel, newCmd ) =
                    case questionnaireMsg of
                        Questionnaire.SetPhase phaseUuid ->
                            applyAction questionnaireSeed identity <|
                                \uuid ->
                                    QuestionnaireEvent.SetPhase
                                        { uuid = uuid
                                        , phaseUuid = Uuid.fromString phaseUuid
                                        , createdAt = createdAt
                                        , createdBy = createdBy
                                        }

                        Questionnaire.SetReply path reply ->
                            applyAction questionnaireSeed identity <|
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

                        Questionnaire.CommentThreadDelete path threadUuid private ->
                            let
                                deleteCommentThread questionnaire =
                                    Questionnaire.deleteCommentThread path threadUuid questionnaire
                            in
                            applyAction questionnaireSeed deleteCommentThread <|
                                \uuid ->
                                    QuestionnaireEvent.DeleteCommentThread
                                        { uuid = uuid
                                        , path = path
                                        , threadUuid = threadUuid
                                        , private = private
                                        , createdAt = createdAt
                                        , createdBy = createdBy
                                        }

                        Questionnaire.CommentThreadResolve path threadUuid private ->
                            let
                                resolveCommentThread questionnaire =
                                    Questionnaire.resolveCommentThread path threadUuid questionnaire
                            in
                            applyAction questionnaireSeed resolveCommentThread <|
                                \uuid ->
                                    QuestionnaireEvent.ResolveCommentThread
                                        { uuid = uuid
                                        , path = path
                                        , threadUuid = threadUuid
                                        , private = private
                                        , createdAt = createdAt
                                        , createdBy = createdBy
                                        }

                        Questionnaire.CommentThreadReopen path threadUuid private ->
                            let
                                reopenCommentThread questionnaire =
                                    Questionnaire.reopenCommentThread path threadUuid questionnaire
                            in
                            applyAction questionnaireSeed reopenCommentThread <|
                                \uuid ->
                                    QuestionnaireEvent.ReopenCommentThread
                                        { uuid = uuid
                                        , path = path
                                        , threadUuid = threadUuid
                                        , private = private
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
            let
                ( summaryReportModel, summaryReportCmd ) =
                    SummaryReport.update summaryReportMsg appState model.summaryReportModel
            in
            withSeed <|
                ( { model | summaryReportModel = summaryReportModel }
                , Cmd.map (wrapMsg << SummaryReportMsg) summaryReportCmd
                )

        DocumentsMsg documentsMsg ->
            case model.questionnaireModel of
                Success qm ->
                    let
                        ( documentsModel, documentsCmd ) =
                            Documents.update (wrapMsg << DocumentsMsg) documentsMsg appState qm.questionnaire.uuid model.documentsModel
                    in
                    withSeed <|
                        ( { model | documentsModel = documentsModel }
                        , documentsCmd
                        )

                _ ->
                    withSeed ( model, Cmd.none )

        NewDocumentMsg newDocumentMsg ->
            case model.questionnaireModel of
                Success qm ->
                    let
                        ( newDocumentModel, newDocumentCmd ) =
                            NewDocument.update
                                { wrapMsg = wrapMsg << NewDocumentMsg
                                , questionnaireUuid = qm.questionnaire.uuid
                                , packageId = qm.questionnaire.package.id
                                , documentsNavigateCmd = cmdNavigate appState <| Routes.projectsDetailDocuments qm.questionnaire.uuid
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

        GetQuestionnaireComplete result ->
            case result of
                Ok questionnaire ->
                    let
                        questionnaireModel =
                            Success <| Questionnaire.init appState questionnaire

                        ( newModel, fetchCmd ) =
                            fetchSubrouteDataFromAfter wrapMsg
                                appState
                                { model | questionnaireModel = questionnaireModel }
                    in
                    withSeed <|
                        ( newModel
                        , Cmd.batch
                            [ WebSocket.open model.websocket
                            , fetchCmd
                            ]
                        )

                Err error ->
                    case ( error, Session.exists appState.session ) of
                        ( BadStatus 403 _, False ) ->
                            let
                                questionnaireRoute =
                                    Routing.toUrl appState (Routes.projectsDetailQuestionnaire model.uuid)

                                loginRoute =
                                    Routes.publicLogin (Just questionnaireRoute)
                            in
                            withSeed <|
                                ( model, cmdNavigate appState loginRoute )

                        _ ->
                            withSeed <|
                                ( { model | questionnaireModel = ApiError.toActionResult appState (lg "apiError.questionnaires.getError" appState) error }
                                , Cmd.none
                                )

        WebSocketMsg wsMsg ->
            handleWebsocketMsg wsMsg appState model

        WebSocketPing ->
            withSeed ( model, WebSocket.ping model.websocket )

        OnlineUserMsg index ouMsg ->
            withSeed <| handleOnlineUserMsg index ouMsg model

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

        SettingsMsg settingsMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << SettingsMsg
                    , redirectCmd = cmdNavigate appState Routes.projectsIndex
                    , packageId = ActionResult.unwrap "" (.questionnaire >> .package >> .id) model.questionnaireModel
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
            case model.questionnaireModel of
                Success questionnaireModel ->
                    let
                        questionnaireDetail =
                            questionnaireModel.questionnaire

                        member =
                            { uuid = Maybe.unwrap Uuid.nil .uuid appState.session.user
                            , firstName = ""
                            , lastName = ""
                            , gravatarHash = ""
                            , imageUrl = Nothing
                            , type_ = ""
                            }

                        ( uuid, newSeed ) =
                            getUuid appState.seed

                        permission =
                            { uuid = uuid
                            , questionnaireUuid = questionnaireDetail.uuid
                            , member = member
                            , perms = [ "VIEW", "EDIT", "ADMIN" ]
                            }

                        detail =
                            { questionnaireDetail | permissions = [ permission ] }

                        questionnaireEditForm =
                            QuestionnaireEditForm.init appState detail

                        cmd =
                            case Form.getOutput questionnaireEditForm of
                                Just form ->
                                    Cmd.map wrapMsg <|
                                        QuestionnairesApi.putQuestionnaire questionnaireDetail.uuid
                                            (QuestionnaireEditForm.encode form)
                                            appState
                                            PutQuestionnaireComplete

                                _ ->
                                    Cmd.none
                    in
                    ( newSeed, { model | addingToMyProjects = Loading }, cmd )

                _ ->
                    ( appState.seed, model, Cmd.none )

        PutQuestionnaireComplete result ->
            case result of
                Ok _ ->
                    ( appState.seed, model, Ports.refresh () )

                Err error ->
                    ( appState.seed, { model | addingToMyProjects = ApiError.toActionResult appState (lg "apiError.questionnaires.putError" appState) error }, Cmd.none )


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
    in
    case WebSocket.receive ServerQuestionnaireAction.decoder websocketMsg model.websocket of
        WebSocket.Message serverAction ->
            case serverAction of
                WebSocketServerAction.Success message ->
                    case message of
                        ServerQuestionnaireAction.SetUserList users ->
                            ( appState.seed, { model | onlineUsers = List.map OnlineUser.init users }, Cmd.none )

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
                                    updateQuestionnaire event data.uuid (Questionnaire.resolveCommentThread data.path data.threadUuid)

                                QuestionnaireEvent.ReopenCommentThread data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.reopenCommentThread data.path data.threadUuid)

                                QuestionnaireEvent.DeleteCommentThread data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.deleteCommentThread data.path data.threadUuid)

                                QuestionnaireEvent.AddComment data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.addComment data.path data.threadUuid data.private (AddCommentData.toComment data))

                                QuestionnaireEvent.EditComment data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.editComment data.path data.threadUuid data.commentUuid data.createdAt data.text)

                                QuestionnaireEvent.DeleteComment data ->
                                    updateQuestionnaire event data.uuid (Questionnaire.deleteComment data.path data.threadUuid data.commentUuid)

                WebSocketServerAction.Error ->
                    ( appState.seed, { model | error = True }, Cmd.none )

        WebSocket.Close ->
            ( appState.seed, { model | offline = True }, Cmd.none )

        _ ->
            ( appState.seed, model, Cmd.none )


handleOnlineUserMsg : Int -> OnlineUser.Msg -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleOnlineUserMsg index msg model =
    ( { model | onlineUsers = List.updateAt index (OnlineUser.update msg) model.onlineUsers }, Cmd.none )
