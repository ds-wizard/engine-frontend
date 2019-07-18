module Questionnaires.Edit.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Questionnaires as QuestionnairesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Form
import Msgs
import Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Questionnaires.Common.QuestionnaireEditForm as QuestionnaireEditForm
import Questionnaires.Edit.Models exposing (Model)
import Questionnaires.Edit.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))
import Routing exposing (cmdNavigate)


fetchData : AppState -> String -> Cmd Msg
fetchData appState uuid =
    QuestionnairesApi.getQuestionnaire uuid appState GetQuestionnaireCompleted


update : (Msg -> Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        FormMsg formMsg ->
            handleForm wrapMsg formMsg appState model

        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted model result

        PutQuestionnaireCompleted result ->
            handlePutQuestionnaireCompleted appState model result



-- Handlers


handleForm : (Msg -> Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleForm wrapMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.editForm, model.questionnaire ) of
        ( Form.Submit, Just editForm, Success questionnaire ) ->
            let
                body =
                    QuestionnaireEditForm.encode questionnaire editForm

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
                    Form.update QuestionnaireEditForm.validation formMsg model.editForm
            in
            ( { model | editForm = editForm }, Cmd.none )


handleGetQuestionnaireCompleted : Model -> Result ApiError QuestionnaireDetail -> ( Model, Cmd Msgs.Msg )
handleGetQuestionnaireCompleted model result =
    case result of
        Ok questionnaire ->
            ( { model
                | editForm = QuestionnaireEditForm.init questionnaire
                , questionnaire = Success questionnaire
              }
            , Cmd.none
            )

        Err error ->
            ( { model | questionnaire = getServerError error "Unable to get questionnaire detail." }
            , getResultCmd result
            )


handlePutQuestionnaireCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
handlePutQuestionnaireCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState.key <| Routing.Questionnaires Index )

        Err error ->
            ( { model | savingQuestionnaire = getServerError error "Questionnaire could not be saved." }
            , getResultCmd result
            )
