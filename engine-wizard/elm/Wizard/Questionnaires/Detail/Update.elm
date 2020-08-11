module Wizard.Questionnaires.Detail.Update exposing
    ( fetchData
    , onUnload
    , update
    )

import ActionResult exposing (ActionResult(..))
import Random exposing (Seed)
import Shared.Api.Levels as LevelsApi
import Shared.Api.Metrics as MetricsApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Auth.Session as Session
import Shared.Data.KnowledgeModel as KnowledgeModel
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.WebSockets.ClientQuestionnaireAction as ClientQuestionnaireAction
import Shared.Data.WebSockets.ServerQuestionnaireAction as ServerQuestionnaireAction
import Shared.Data.WebSockets.WebSocketServerAction as WebSocketServerAction
import Shared.Error.ApiError as ApiError exposing (ApiError(..))
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setLevels, setMetrics)
import Shared.WebSocket as WebSocket
import String
import Triple
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.OnlineUser as OnlineUser
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Public.Routes exposing (Route(..))
import Wizard.Questionnaires.Common.CloneQuestionnaireModal.Msgs as CloneQuestionnaireModal
import Wizard.Questionnaires.Common.CloneQuestionnaireModal.Update as CloneQuestionnaireModal
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Msgs as DeleteQuestionnaireModalMsgs
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Update as DeleteQuestionnaireModal
import Wizard.Questionnaires.Detail.Components.QuestionnaireSaving as QuestionnaireSaving
import Wizard.Questionnaires.Detail.Models exposing (Model, addSavingActionUuid, removeSavingActionUuid)
import Wizard.Questionnaires.Detail.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState uuid =
    Cmd.batch
        [ QuestionnairesApi.getQuestionnaire uuid appState GetQuestionnaireComplete
        , LevelsApi.getLevels appState GetLevelsComplete
        , MetricsApi.getMetrics appState GetMetricsComplete
        ]


onUnload : Model -> Cmd msg
onUnload model =
    WebSocket.close model.websocket


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    let
        withSeed ( m, c ) =
            ( appState.seed, m, c )

        wrap m =
            ( appState.seed, m, Cmd.none )
    in
    case msg of
        GetQuestionnaireComplete result ->
            withSeed <| handleGetQuestionnaireComplete appState model result

        GetLevelsComplete result ->
            withSeed <| handleGetLevelsComplete appState model result

        GetMetricsComplete result ->
            withSeed <| handleGetMetricsComplete appState model result

        WebSocketMsg wsMsg ->
            handleWebsocketMsg wsMsg appState model

        WebSocketPing _ ->
            withSeed ( model, WebSocket.ping model.websocket )

        OnlineUserMsg index ouMsg ->
            withSeed <| handleOnlineUserMsg index ouMsg model

        ActionsDropdownMsg state ->
            wrap { model | actionsDropdownState = state }

        QuestionnaireSavingMsg qsMsg ->
            wrap { model | questionnaireSavingModel = QuestionnaireSaving.update qsMsg model.questionnaireSavingModel }

        QuestionnaireMsg questionnaireMsg ->
            handleQuestionnaireMsg wrapMsg appState model questionnaireMsg

        DeleteQuestionnaireModalMsg modalMsg ->
            withSeed <| handleDeleteQuestionnaireModalMsg wrapMsg modalMsg appState model

        CloneQuestionnaireModalMsg modalMsg ->
            withSeed <| handleCloneQuestionnaireModalMsg wrapMsg modalMsg appState model

        Refresh ->
            withSeed ( model, Ports.refresh () )


handleGetQuestionnaireComplete : AppState -> Model -> Result ApiError QuestionnaireDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetQuestionnaireComplete appState model result =
    case result of
        Ok questionnaire ->
            let
                questionnaireModel =
                    Success <| Questionnaire.init questionnaire

                questionnaireModelWithChapter =
                    case List.head (KnowledgeModel.getChapters questionnaire.knowledgeModel) of
                        Just chapter ->
                            ActionResult.map (Questionnaire.setActiveChapterUuid chapter.uuid) questionnaireModel

                        Nothing ->
                            questionnaireModel
            in
            ( { model | questionnaireModel = questionnaireModelWithChapter }
            , WebSocket.open model.websocket
            )

        Err error ->
            case ( error, Session.exists appState.session ) of
                ( BadStatus 403 _, False ) ->
                    let
                        questionnaireRoute =
                            Routing.toUrl appState
                                (Routes.QuestionnairesRoute (DetailRoute model.uuid))

                        loginRoute =
                            Routes.PublicRoute (LoginRoute (Just questionnaireRoute))
                    in
                    ( model, cmdNavigate appState loginRoute )

                _ ->
                    ( { model | questionnaireModel = ApiError.toActionResult (lg "apiError.questionnaires.getError" appState) error }
                    , Cmd.none
                    )


handleGetLevelsComplete : AppState -> Model -> Result ApiError (List Level) -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetLevelsComplete appState model result =
    applyResult
        { setResult = setLevels
        , defaultError = lg "apiError.levels.getListError" appState
        , model = model
        , result = result
        }


handleGetMetricsComplete : AppState -> Model -> Result ApiError (List Metric) -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetMetricsComplete appState model result =
    applyResult
        { setResult = setMetrics
        , defaultError = lg "apiError.metrics.getListError" appState
        , model = model
        , result = result
        }


handleWebsocketMsg : WebSocket.RawMsg -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
handleWebsocketMsg websocketMsg appState model =
    let
        updateQuestionnaire actionUuid fn =
            let
                newQuestionnaire =
                    if not removed then
                        ActionResult.map fn model.questionnaireModel

                    else
                        model.questionnaireModel

                ( newModel, removed ) =
                    removeSavingActionUuid actionUuid model
            in
            ( appState.seed, { newModel | questionnaireModel = newQuestionnaire }, Cmd.none )
    in
    case WebSocket.receive ServerQuestionnaireAction.decoder websocketMsg model.websocket of
        WebSocket.Message serverAction ->
            case serverAction of
                WebSocketServerAction.Success message ->
                    case message of
                        ServerQuestionnaireAction.SetUserList users ->
                            ( appState.seed, { model | onlineUsers = List.map OnlineUser.init users }, Cmd.none )

                        ServerQuestionnaireAction.SetReply data ->
                            updateQuestionnaire data.uuid (Questionnaire.setReply data.path data.value)

                        ServerQuestionnaireAction.ClearReply data ->
                            updateQuestionnaire data.uuid (Questionnaire.clearReply data.path)

                        ServerQuestionnaireAction.SetLevel data ->
                            updateQuestionnaire data.uuid (Questionnaire.setLevel data.level)

                        ServerQuestionnaireAction.SetLabels data ->
                            updateQuestionnaire data.uuid (Questionnaire.setLabels data.path data.value)

                WebSocketServerAction.Error ->
                    ( appState.seed, { model | error = True }, Cmd.none )

        WebSocket.Close ->
            ( appState.seed, { model | offline = True }, Cmd.none )

        _ ->
            ( appState.seed, model, Cmd.none )


handleOnlineUserMsg : Int -> OnlineUser.Msg -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleOnlineUserMsg index msg model =
    let
        updateUser i user =
            if i == index then
                OnlineUser.update msg user

            else
                user
    in
    ( { model | onlineUsers = List.indexedMap updateUser model.onlineUsers }, Cmd.none )


handleQuestionnaireMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Questionnaire.Msg -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
handleQuestionnaireMsg wrapMsg appState model questionnaireMsg =
    let
        ( newSeed1, newQuestionnaireModel, questionnaireCmd ) =
            case ( model.questionnaireModel, model.levels, model.metrics ) of
                ( Success questionnaireModel, Success levels, Success metrics ) ->
                    Triple.mapSnd Success <|
                        Questionnaire.update
                            questionnaireMsg
                            appState
                            { levels = levels, metrics = metrics }
                            questionnaireModel

                _ ->
                    ( appState.seed, model.questionnaireModel, Cmd.none )

        newModel1 =
            { model | questionnaireModel = newQuestionnaireModel }

        applyAction buildAction =
            let
                ( uuid, newSeed2 ) =
                    Random.step Uuid.uuidGenerator newSeed1

                newModel2 =
                    addSavingActionUuid uuid newModel1

                cmd =
                    WebSocket.send model.websocket (ClientQuestionnaireAction.encode (buildAction uuid))
            in
            ( newSeed2, newModel2, cmd )

        ( newSeed, newModel, newCmd ) =
            case questionnaireMsg of
                Questionnaire.SetLevel levelString ->
                    applyAction <|
                        \uuid ->
                            ClientQuestionnaireAction.SetLevel
                                { uuid = uuid
                                , level = Maybe.withDefault 1 (String.toInt levelString)
                                }

                Questionnaire.SetReply path value ->
                    applyAction <|
                        \uuid ->
                            ClientQuestionnaireAction.SetReply
                                { uuid = uuid
                                , path = path
                                , value = value
                                }

                Questionnaire.ClearReply path ->
                    applyAction <|
                        \uuid ->
                            ClientQuestionnaireAction.ClearReply
                                { uuid = uuid
                                , path = path
                                }

                Questionnaire.SetLabels path value ->
                    applyAction <|
                        \uuid ->
                            ClientQuestionnaireAction.SetLabels
                                { uuid = uuid
                                , path = path
                                , value = value
                                }

                _ ->
                    ( appState.seed, newModel1, Cmd.none )
    in
    ( newSeed
    , newModel
    , Cmd.batch [ Cmd.map (wrapMsg << QuestionnaireMsg) questionnaireCmd, newCmd ]
    )


handleDeleteQuestionnaireModalMsg : (Msg -> Wizard.Msgs.Msg) -> DeleteQuestionnaireModalMsgs.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteQuestionnaireModalMsg wrapMsg modalMsg appState model =
    let
        updateConfig =
            { wrapMsg = wrapMsg << DeleteQuestionnaireModalMsg
            , deleteCompleteCmd =
                cmdNavigate appState (Routes.QuestionnairesRoute (IndexRoute PaginationQueryString.empty))
            }

        ( deleteModalModel, cmd ) =
            DeleteQuestionnaireModal.update updateConfig modalMsg appState model.deleteModalModel
    in
    ( { model | deleteModalModel = deleteModalModel }
    , cmd
    )


handleCloneQuestionnaireModalMsg : (Msg -> Wizard.Msgs.Msg) -> CloneQuestionnaireModal.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleCloneQuestionnaireModalMsg wrapMsg modalMsg appState model =
    let
        updateConfig =
            { wrapMsg = wrapMsg << CloneQuestionnaireModalMsg
            , cloneCompleteCmd =
                cmdNavigate appState << Routes.QuestionnairesRoute << DetailRoute << .uuid
            }

        ( cloneModalModel, cmd ) =
            CloneQuestionnaireModal.update updateConfig modalMsg appState model.cloneModalModel
    in
    ( { model | cloneModalModel = cloneModalModel }
    , cmd
    )
