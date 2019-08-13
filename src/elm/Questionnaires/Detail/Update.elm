module Questionnaires.Detail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (applyResult, getResultCmd)
import Common.Api.Levels as LevelsApi
import Common.Api.Metrics as MetricsApi
import Common.Api.Questionnaires as QuestionnairesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Questionnaire.Models exposing (initialModel, updateReplies)
import Common.Questionnaire.Msgs
import Common.Questionnaire.Update
import Common.Setters exposing (setLevels, setMetrics, setQuestionnaireDetail)
import KMEditor.Common.KnowledgeModel.Level exposing (Level)
import KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import Msgs
import Questionnaires.Common.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Questionnaires.Detail.Models exposing (Model)
import Questionnaires.Detail.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))
import Routing exposing (cmdNavigate)


fetchData : AppState -> String -> Cmd Msg
fetchData appState uuid =
    Cmd.batch
        [ QuestionnairesApi.getQuestionnaire uuid appState GetQuestionnaireCompleted
        , LevelsApi.getLevels appState GetLevelsCompleted
        , MetricsApi.getMetrics appState GetMetricsCompleted
        ]


update : (Msg -> Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted appState model result

        GetLevelsCompleted result ->
            handleGetLevelsCompleted model result

        GetMetricsCompleted result ->
            handleGetMetricsCompleted appState model result

        QuestionnaireMsg qMsg ->
            handleQuestionnaireMsg wrapMsg qMsg appState model

        Save ->
            handleSave wrapMsg appState model

        PutRepliesCompleted result ->
            handlePutRepliesCompleted appState model result



-- Handlers


handleGetQuestionnaireCompleted : AppState -> Model -> Result ApiError QuestionnaireDetail -> ( Model, Cmd Msgs.Msg )
handleGetQuestionnaireCompleted appState model result =
    initQuestionnaireModel appState <|
        applyResult
            { setResult = setQuestionnaireDetail
            , defaultError = "Unable to get questionnaire."
            , model = model
            , result = result
            }


handleGetLevelsCompleted : Model -> Result ApiError (List Level) -> ( Model, Cmd Msgs.Msg )
handleGetLevelsCompleted model result =
    applyResult
        { setResult = setLevels
        , defaultError = "Unable to get levels."
        , model = model
        , result = result
        }


handleGetMetricsCompleted : AppState -> Model -> Result ApiError (List Metric) -> ( Model, Cmd Msgs.Msg )
handleGetMetricsCompleted appState model result =
    initQuestionnaireModel appState <|
        applyResult
            { setResult = setMetrics
            , defaultError = "Unable to get metrics."
            , model = model
            , result = result
            }


handleQuestionnaireMsg : (Msg -> Msgs.Msg) -> Common.Questionnaire.Msgs.Msg -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleQuestionnaireMsg wrapMsg msg appState model =
    let
        ( newQuestionnaireModel, cmd ) =
            case model.questionnaireModel of
                Success qm ->
                    let
                        ( questionnaireModel, questionnaireCmd ) =
                            Common.Questionnaire.Update.update msg appState qm
                    in
                    ( Success questionnaireModel, questionnaireCmd )

                _ ->
                    ( model.questionnaireModel, Cmd.none )
    in
    ( { model | questionnaireModel = newQuestionnaireModel }, cmd |> Cmd.map (QuestionnaireMsg >> wrapMsg) )


handleSave : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
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


handlePutRepliesCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
handlePutRepliesCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState.key <| Routing.Questionnaires Index )

        Err error ->
            ( { model | savingQuestionnaire = getServerError error "Questionnaire could not be saved." }
            , getResultCmd result
            )



-- Helpers


initQuestionnaireModel : AppState -> ( Model, Cmd Msgs.Msg ) -> ( Model, Cmd Msgs.Msg )
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
