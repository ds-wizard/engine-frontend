module Wizard.Projects.Detail.Update exposing (fetchData, onUnload, update)

import ActionResult exposing (ActionResult(..))
import Random exposing (Seed)
import Shared.Api.Levels as LevelsApi
import Shared.Api.Metrics as MetricsApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Auth.Session as Session
import Shared.Data.KnowledgeModel as KnowledgeModel
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.WebSockets.ClientQuestionnaireAction as ClientQuestionnaireAction
import Shared.Data.WebSockets.ServerQuestionnaireAction as ServerQuestionnaireAction
import Shared.Data.WebSockets.WebSocketServerAction as WebSocketServerAction
import Shared.Error.ApiError as ApiError exposing (ApiError(..))
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setLevels, setMetrics)
import Shared.WebSocket as WebSocket
import Triple
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.OnlineUser as OnlineUser
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.SummaryReport as SummaryReport
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Projects.Detail.Components.NewDocument as NewDocument
import Wizard.Projects.Detail.Components.PlanSaving as PlanSaving
import Wizard.Projects.Detail.Components.Preview as Preview
import Wizard.Projects.Detail.Components.Settings as Settings
import Wizard.Projects.Detail.Components.ShareModal as ShareModal
import Wizard.Projects.Detail.Documents.Update as Documents
import Wizard.Projects.Detail.Models exposing (Model, addSavingActionUuid, hasTemplate, initPageModel, removeSavingActionUuid)
import Wizard.Projects.Detail.Msgs exposing (Msg(..))
import Wizard.Projects.Detail.PlanDetailRoute as PlanDetailRoute
import Wizard.Projects.Routes exposing (Route(..))
import Wizard.Public.Routes exposing (Route(..))
import Wizard.Routes as Routes exposing (Route(..))
import Wizard.Routing as Routing exposing (cmdNavigate)


fetchData : AppState -> Uuid -> Model -> Cmd Msg
fetchData appState uuid model =
    if ActionResult.unwrap False (.uuid >> (==) uuid) model.questionnaireModel then
        Cmd.batch
            [ fetchSubrouteData appState model
            , WebSocket.open model.websocket
            ]

    else
        Cmd.batch
            [ QuestionnairesApi.getQuestionnaire uuid appState GetQuestionnaireComplete
            , LevelsApi.getLevels appState GetLevelsComplete
            , MetricsApi.getMetrics appState GetMetricsComplete
            ]


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
                        SummaryReport.fetchData2 appState uuid

                PlanDetailRoute.Documents _ ->
                    Cmd.map DocumentsMsg <|
                        Documents.fetchData

                PlanDetailRoute.NewDocument ->
                    Cmd.map NewDocumentMsg <|
                        NewDocument.fetchData appState uuid

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


fetchSubrouteDataFromAfter : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
fetchSubrouteDataFromAfter wrapMsg appState model =
    case ( ActionResult.combine3 model.questionnaireModel model.metrics model.levels, appState.route ) of
        ( Success _, ProjectsRoute (DetailRoute _ route) ) ->
            ( initPageModel route model, Cmd.map wrapMsg <| fetchSubrouteData appState model )

        _ ->
            ( model, Cmd.none )


onUnload : Routes.Route -> Model -> Cmd msg
onUnload newRoute model =
    case newRoute of
        ProjectsRoute (DetailRoute uuid _) ->
            if uuid == model.uuid then
                Cmd.none

            else
                WebSocket.close model.websocket

        _ ->
            WebSocket.close model.websocket


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    let
        withSeed ( m, c ) =
            ( appState.seed, m, c )
    in
    case msg of
        QuestionnaireMsg questionnaireMsg ->
            let
                ( newSeed1, newQuestionnaireModel, questionnaireCmd ) =
                    case ( model.questionnaireModel, model.levels, model.metrics ) of
                        ( Success questionnaireModel, Success levels, Success metrics ) ->
                            Triple.mapSnd Success <|
                                Questionnaire.update questionnaireMsg appState { levels = levels, metrics = metrics, events = [] } questionnaireModel

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
            case ( model.questionnaireModel, model.metrics ) of
                ( Success qm, Success metrics ) ->
                    let
                        ( summaryReportModel, summaryReportCmd ) =
                            SummaryReport.update summaryReportMsg appState { questionnaire = qm.questionnaire, metrics = metrics } model.summaryReportModel
                    in
                    withSeed <|
                        ( { model | summaryReportModel = summaryReportModel }
                        , Cmd.map (wrapMsg << SummaryReportMsg) summaryReportCmd
                        )

                _ ->
                    withSeed ( model, Cmd.none )

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
                                , documentsNavigateCmd = cmdNavigate appState <| ProjectsRoute <| DetailRoute qm.questionnaire.uuid <| PlanDetailRoute.Documents PaginationQueryString.empty
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
                            Success <| Questionnaire.init questionnaire

                        questionnaireModelWithChapter =
                            case List.head (KnowledgeModel.getChapters questionnaire.knowledgeModel) of
                                Just chapter ->
                                    ActionResult.map (Questionnaire.setActiveChapterUuid chapter.uuid) questionnaireModel

                                Nothing ->
                                    questionnaireModel

                        ( newModel, fetchCmd ) =
                            fetchSubrouteDataFromAfter wrapMsg
                                appState
                                { model
                                    | questionnaireModel = questionnaireModelWithChapter
                                    , shareModalModel = ShareModal.setQuestionnaire questionnaire model.shareModalModel
                                }
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
                                    Routing.toUrl appState
                                        (ProjectsRoute (DetailRoute model.uuid PlanDetailRoute.Questionnaire))

                                loginRoute =
                                    PublicRoute (LoginRoute (Just questionnaireRoute))
                            in
                            withSeed <|
                                ( model, cmdNavigate appState loginRoute )

                        _ ->
                            withSeed <|
                                ( { model | questionnaireModel = ApiError.toActionResult (lg "apiError.questionnaires.getError" appState) error }
                                , Cmd.none
                                )

        GetLevelsComplete result ->
            let
                ( newModel1, cmd ) =
                    applyResult
                        { setResult = setLevels
                        , defaultError = lg "apiError.levels.getListError" appState
                        , model = model
                        , result = result
                        }

                ( newModel, fetchCmd ) =
                    fetchSubrouteDataFromAfter wrapMsg appState newModel1
            in
            withSeed <|
                ( newModel
                , Cmd.batch [ cmd, fetchCmd ]
                )

        GetMetricsComplete result ->
            let
                ( newModel1, cmd ) =
                    applyResult
                        { setResult = setMetrics
                        , defaultError = lg "apiError.metrics.getListError" appState
                        , model = model
                        , result = result
                        }

                ( newModel, fetchCmd ) =
                    fetchSubrouteDataFromAfter wrapMsg appState newModel1
            in
            withSeed <|
                ( newModel
                , Cmd.batch [ cmd, fetchCmd ]
                )

        WebSocketMsg wsMsg ->
            handleWebsocketMsg wsMsg appState model

        WebSocketPing _ ->
            withSeed ( model, WebSocket.ping model.websocket )

        ScrollToTodo todo ->
            case model.questionnaireModel of
                Success questionnaireModel ->
                    let
                        newQuestionnaireModel =
                            Questionnaire.setActiveChapterUuid todo.chapter.uuid questionnaireModel

                        selector =
                            "[data-path=\"" ++ todo.path ++ "\"]"
                    in
                    ( appState.seed
                    , { model
                        | questionnaireModel = Success newQuestionnaireModel
                      }
                    , Cmd.batch
                        [ cmdNavigate appState (ProjectsRoute (DetailRoute model.uuid PlanDetailRoute.Questionnaire))
                        , Ports.scrollIntoView selector
                        ]
                    )

                _ ->
                    ( appState.seed, model, Cmd.none )

        OnlineUserMsg index ouMsg ->
            withSeed <| handleOnlineUserMsg index ouMsg model

        PlanSavingMsg qsMsg ->
            withSeed ( { model | planSavingModel = PlanSaving.update qsMsg model.planSavingModel }, Cmd.none )

        ShareModalMsg shareModalMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << ShareModalMsg
                    , questionnaireUuid = model.uuid
                    , permissions = ActionResult.unwrap [] (.questionnaire >> .permissions) model.questionnaireModel
                    }

                ( shareModalModel, cmd ) =
                    ShareModal.update updateConfig shareModalMsg appState model.shareModalModel
            in
            withSeed ( { model | shareModalModel = shareModalModel }, cmd )

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
            ( appState.seed
            , { newModel | questionnaireModel = newQuestionnaire }
            , Cmd.none
            )
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
