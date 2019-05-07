module Questionnaires.Detail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (Session)
import Common.Api exposing (getResultCmd)
import Common.Api.Levels as LevelsApi
import Common.Api.Metrics as MetricsApi
import Common.Api.Questionnaires as QuestionnairesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Questionnaire.Models exposing (QuestionnaireDetail, encodeQuestionnaireDetail, initialModel, updateReplies)
import Common.Questionnaire.Msgs
import Common.Questionnaire.Update
import KMEditor.Common.Models.Entities exposing (Level, Metric)
import Msgs
import Questionnaires.Detail.Models exposing (Model)
import Questionnaires.Detail.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))
import Routing exposing (cmdNavigate)


fetchData : (Msg -> Msgs.Msg) -> AppState -> String -> Cmd Msgs.Msg
fetchData wrapMsg appState uuid =
    Cmd.map wrapMsg <|
        Cmd.batch
            [ QuestionnairesApi.getQuestionnaire uuid appState GetQuestionnaireCompleted
            , LevelsApi.getLevels appState GetLevelsCompleted
            , MetricsApi.getMetrics appState GetMetricsCompleted
            ]


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted model result

        GetLevelsCompleted result ->
            handleGetLevelsCompleted model result

        GetMetricsCompleted result ->
            handleGetMetricsCompleted model result

        QuestionnaireMsg qMsg ->
            handleQuestionnaireMsg wrapMsg qMsg appState model

        Save ->
            handleSave wrapMsg appState model

        PutRepliesCompleted result ->
            handlePutRepliesCompleted appState model result


handleGetQuestionnaireCompleted : Model -> Result ApiError QuestionnaireDetail -> ( Model, Cmd Msgs.Msg )
handleGetQuestionnaireCompleted model result =
    let
        newModel =
            case result of
                Ok questionnaireDetail ->
                    { model | questionnaireDetail = Success questionnaireDetail }

                Err error ->
                    { model | questionnaireDetail = getServerError error "Unable to get questionnaire." }

        cmd =
            getResultCmd result
    in
    ( initQuestionnaireModel newModel, cmd )


handleGetLevelsCompleted : Model -> Result ApiError (List Level) -> ( Model, Cmd Msgs.Msg )
handleGetLevelsCompleted model result =
    let
        newModel =
            case result of
                Ok levels ->
                    { model | levels = Success levels }

                Err error ->
                    { model | levels = getServerError error "Unable to get levels." }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


initQuestionnaireModel : Model -> Model
initQuestionnaireModel model =
    case ( model.questionnaireDetail, model.metrics ) of
        ( Success questionnaireDetail, Success metrics ) ->
            { model | questionnaireModel = Success <| initialModel questionnaireDetail metrics [] }

        ( Error err, _ ) ->
            { model | questionnaireModel = Error err }

        ( _, Error err ) ->
            { model | questionnaireModel = Error err }

        _ ->
            model


handleGetMetricsCompleted : Model -> Result ApiError (List Metric) -> ( Model, Cmd Msgs.Msg )
handleGetMetricsCompleted model result =
    let
        newModel =
            case result of
                Ok metrics ->
                    { model | metrics = Success metrics }

                Err error ->
                    { model | metrics = getServerError error "Unable to get metrics." }

        cmd =
            getResultCmd result
    in
    ( initQuestionnaireModel newModel, cmd )


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
                    encodeQuestionnaireDetail newQuestionnaireModel.questionnaire

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
