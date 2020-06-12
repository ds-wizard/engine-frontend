module Wizard.Questionnaires.Detail.Update exposing
    ( fetchData
    , isGuarded
    , update
    )

import ActionResult exposing (ActionResult(..))
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (l, lg, lgf)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.Api.Levels as LevelsApi
import Wizard.Common.Api.Metrics as MetricsApi
import Wizard.Common.Api.Questionnaires as QuestionnairesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Pagination.PaginationQueryString as PaginationQueryString
import Wizard.Common.Questionnaire.Models exposing (cleanDirty, initialModel, updateReplies)
import Wizard.Common.Questionnaire.Msgs
import Wizard.Common.Questionnaire.Update
import Wizard.Common.Setters exposing (setLevels, setMetrics, setQuestionnaireDetail)
import Wizard.KMEditor.Common.KnowledgeModel.Level exposing (Level)
import Wizard.KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Msgs as DeleteQuestionnaireModalMsgs
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Update as DeleteQuestionnaireModal
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Wizard.Questionnaires.Common.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Wizard.Questionnaires.Detail.Models exposing (Model, isDirty)
import Wizard.Questionnaires.Detail.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Questionnaires.Detail.Update"


fetchData : AppState -> String -> Cmd Msg
fetchData appState uuid =
    Cmd.batch
        [ QuestionnairesApi.getQuestionnaire uuid appState GetQuestionnaireCompleted
        , LevelsApi.getLevels appState GetLevelsCompleted
        , MetricsApi.getMetrics appState GetMetricsCompleted
        ]


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    if isDirty model then
        Just <| l_ "unsavedChanges" appState

    else
        Nothing


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted appState model result

        GetLevelsCompleted result ->
            handleGetLevelsCompleted appState model result

        GetMetricsCompleted result ->
            handleGetMetricsCompleted appState model result

        QuestionnaireMsg qMsg ->
            handleQuestionnaireMsg wrapMsg qMsg appState model

        Save ->
            handleSave wrapMsg appState model

        PutRepliesCompleted result ->
            handlePutRepliesCompleted appState model result

        Discard ->
            handleDiscard wrapMsg appState model

        ActionsDropdownMsg state ->
            ( { model | actionsDropdownState = state }, Cmd.none )

        DeleteQuestionnaireModalMsg modalMsg ->
            handleDeleteQuestionnaireModalMsg wrapMsg modalMsg appState model

        CloneQuestionnaire questionnaire ->
            handleCloneQuestionnaire wrapMsg appState model questionnaire

        CloneQuestionnaireCompleted result ->
            handleCloneQuestionnaireCompleted appState model result



-- Handlers


handleGetQuestionnaireCompleted : AppState -> Model -> Result ApiError QuestionnaireDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetQuestionnaireCompleted appState model result =
    initQuestionnaireModel appState <|
        applyResult
            { setResult = setQuestionnaireDetail
            , defaultError = lg "apiError.questionnaires.getError" appState
            , model = model
            , result = result
            }


handleGetLevelsCompleted : AppState -> Model -> Result ApiError (List Level) -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetLevelsCompleted appState model result =
    applyResult
        { setResult = setLevels
        , defaultError = lg "apiError.levels.getListError" appState
        , model = model
        , result = result
        }


handleGetMetricsCompleted : AppState -> Model -> Result ApiError (List Metric) -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetMetricsCompleted appState model result =
    initQuestionnaireModel appState <|
        applyResult
            { setResult = setMetrics
            , defaultError = lg "apiError.metrics.getListError" appState
            , model = model
            , result = result
            }


handleQuestionnaireMsg : (Msg -> Wizard.Msgs.Msg) -> Wizard.Common.Questionnaire.Msgs.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleQuestionnaireMsg wrapMsg msg appState model =
    let
        ( newQuestionnaireModel, cmd ) =
            case model.questionnaireModel of
                Success qm ->
                    let
                        ( questionnaireModel, questionnaireCmd ) =
                            Wizard.Common.Questionnaire.Update.update msg appState qm
                    in
                    ( Success questionnaireModel, questionnaireCmd )

                _ ->
                    ( model.questionnaireModel, Cmd.none )

        newModel =
            { model | questionnaireModel = newQuestionnaireModel }

        setUnloadMsgCmd =
            if isDirty newModel then
                Ports.setUnloadMessage <| l_ "unsavedChanges" appState

            else
                Cmd.none
    in
    ( newModel
    , Cmd.batch
        [ cmd |> Cmd.map (QuestionnaireMsg >> wrapMsg)
        , setUnloadMsgCmd
        ]
    )


handleSave : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleSave wrapMsg appState model =
    case model.questionnaireModel of
        Success questionnaireModel ->
            let
                newQuestionnaireModel =
                    updateReplies questionnaireModel

                body =
                    QuestionnaireDetail.encode newQuestionnaireModel.questionnaire

                cmd =
                    Cmd.map wrapMsg <|
                        QuestionnairesApi.putQuestionnaire model.uuid body appState PutRepliesCompleted
            in
            ( { model | questionnaireModel = Success newQuestionnaireModel, savingQuestionnaire = Loading }, cmd )

        _ ->
            ( model, Cmd.none )


handlePutRepliesCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutRepliesCompleted appState model result =
    case result of
        Ok _ ->
            ( { model
                | questionnaireModel = ActionResult.map cleanDirty model.questionnaireModel
                , savingQuestionnaire = Success ""
              }
            , Ports.clearUnloadMessage ()
            )

        Err error ->
            ( { model | savingQuestionnaire = ApiError.toActionResult (lg "apiError.questionnaires.replies.putError" appState) error }
            , getResultCmd result
            )


handleDiscard : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDiscard wrapMsg appState model =
    ( { model | questionnaireModel = Loading }
    , Cmd.map wrapMsg <| fetchData appState model.uuid
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


handleCloneQuestionnaire : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> QuestionnaireDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleCloneQuestionnaire wrapMsg appState model questionnaire =
    ( { model | cloningQuestionnaire = Loading }
    , QuestionnairesApi.cloneQuestionnaire questionnaire.uuid appState (wrapMsg << CloneQuestionnaireCompleted)
    )


handleCloneQuestionnaireCompleted : AppState -> Model -> Result ApiError Questionnaire -> ( Model, Cmd Wizard.Msgs.Msg )
handleCloneQuestionnaireCompleted appState model result =
    case result of
        Ok questionnaire ->
            ( { model | cloningQuestionnaire = Success <| lgf "apiSuccess.questionnaires.clone" [ questionnaire.name ] appState }
            , cmdNavigate appState (Routes.QuestionnairesRoute (DetailRoute questionnaire.uuid))
            )

        Err error ->
            ( { model | cloningQuestionnaire = ApiError.toActionResult (lg "apiError.questionnaires.cloneError" appState) error }
            , getResultCmd result
            )



-- Helpers


initQuestionnaireModel : AppState -> ( Model, Cmd Wizard.Msgs.Msg ) -> ( Model, Cmd Wizard.Msgs.Msg )
initQuestionnaireModel appState ( model, cmd ) =
    let
        newModel =
            case ( model.questionnaireDetail, model.metrics ) of
                ( Success questionnaireDetail, Success metrics ) ->
                    { model | questionnaireModel = Success <| initialModel appState questionnaireDetail metrics [] }

                ( Error err, _ ) ->
                    { model | questionnaireModel = Error err }

                ( _, Error err ) ->
                    { model | questionnaireModel = Error err }

                _ ->
                    model
    in
    ( newModel, Cmd.batch [ cmd, Ports.clearUnloadMessage () ] )
