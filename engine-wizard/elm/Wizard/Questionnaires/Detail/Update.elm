module Wizard.Questionnaires.Detail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.Api.Levels as LevelsApi
import Wizard.Common.Api.Metrics as MetricsApi
import Wizard.Common.Api.Questionnaires as QuestionnairesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Locale exposing (lg)
import Wizard.Common.Questionnaire.Models exposing (initialModel, updateReplies)
import Wizard.Common.Questionnaire.Msgs
import Wizard.Common.Questionnaire.Update
import Wizard.Common.Setters exposing (setLevels, setMetrics, setQuestionnaireDetail)
import Wizard.KMEditor.Common.KnowledgeModel.Level exposing (Level)
import Wizard.KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import Wizard.Msgs
import Wizard.Questionnaires.Common.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Wizard.Questionnaires.Detail.Models exposing (Model)
import Wizard.Questionnaires.Detail.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> String -> Cmd Msg
fetchData appState uuid =
    Cmd.batch
        [ QuestionnairesApi.getQuestionnaire uuid appState GetQuestionnaireCompleted
        , LevelsApi.getLevels appState GetLevelsCompleted
        , MetricsApi.getMetrics appState GetMetricsCompleted
        ]


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
    in
    ( { model | questionnaireModel = newQuestionnaireModel }, cmd |> Cmd.map (QuestionnaireMsg >> wrapMsg) )


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
            ( { model | questionnaireModel = Success newQuestionnaireModel }, cmd )

        _ ->
            ( model, Cmd.none )


handlePutRepliesCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutRepliesCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState <| Routes.QuestionnairesRoute IndexRoute )

        Err error ->
            ( { model | savingQuestionnaire = ApiError.toActionResult (lg "apiError.questionnaires.replies.putError" appState) error }
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
    ( newModel, cmd )
