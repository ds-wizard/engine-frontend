module Questionnaires.Edit.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Questionnaires as QuestionnairesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import Form
import Msgs
import Questionnaires.Edit.Models exposing (Model, QuestionnaireEditForm, encodeEditForm, initQuestionnaireEditForm, questionnaireEditFormValidation)
import Questionnaires.Edit.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))
import Routing exposing (cmdNavigate)


fetchData : (Msg -> Msgs.Msg) -> AppState -> String -> Cmd Msgs.Msg
fetchData wrapMsg appState uuid =
    Cmd.map wrapMsg <|
        QuestionnairesApi.getQuestionnaire uuid appState GetQuestionnaireCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model

        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted result model

        PutQuestionnaireCompleted result ->
            handlePutQuestionnaireCompleted result appState model


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.editForm, model.questionnaire ) of
        ( Form.Submit, Just editForm, Success questionnaire ) ->
            let
                body =
                    encodeEditForm questionnaire editForm

                cmd =
                    Cmd.map wrapMsg <|
                        QuestionnairesApi.putQuestionnaire model.uuid body appState PutQuestionnaireCompleted
            in
            ( { model | savingQuestionnaire = Loading }
            , cmd
            )

        _ ->
            let
                editForm =
                    Form.update questionnaireEditFormValidation formMsg model.editForm
            in
            ( { model | editForm = editForm }, Cmd.none )


handleGetQuestionnaireCompleted : Result ApiError QuestionnaireDetail -> Model -> ( Model, Cmd Msgs.Msg )
handleGetQuestionnaireCompleted result model =
    case result of
        Ok questionnaire ->
            ( { model
                | editForm = initQuestionnaireEditForm questionnaire
                , questionnaire = Success questionnaire
              }
            , Cmd.none
            )

        Err error ->
            ( { model | questionnaire = getServerError error "Unable to get questionnaire detail." }
            , getResultCmd result
            )


handlePutQuestionnaireCompleted : Result ApiError () -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handlePutQuestionnaireCompleted result appState model =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState.key <| Routing.Questionnaires Index )

        Err error ->
            ( { model | savingQuestionnaire = getServerError error "Questionnaire could not be saved." }
            , getResultCmd result
            )
