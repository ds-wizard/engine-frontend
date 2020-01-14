module Wizard.Questionnaires.Edit.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Form
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.Api.Questionnaires as QuestionnairesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Wizard.Questionnaires.Common.QuestionnaireEditForm as QuestionnaireEditForm
import Wizard.Questionnaires.Edit.Models exposing (Model)
import Wizard.Questionnaires.Edit.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> String -> Cmd Msg
fetchData appState uuid =
    QuestionnairesApi.getQuestionnaire uuid appState GetQuestionnaireCompleted


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        FormMsg formMsg ->
            handleForm wrapMsg formMsg appState model

        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted appState model result

        PutQuestionnaireCompleted result ->
            handlePutQuestionnaireCompleted appState model result



-- Handlers


handleForm : (Msg -> Wizard.Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
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


handleGetQuestionnaireCompleted : AppState -> Model -> Result ApiError QuestionnaireDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetQuestionnaireCompleted appState model result =
    case result of
        Ok questionnaire ->
            ( { model
                | editForm = QuestionnaireEditForm.init questionnaire
                , questionnaire = Success questionnaire
              }
            , Cmd.none
            )

        Err error ->
            ( { model | questionnaire = ApiError.toActionResult (lg "apiError.questionnaires.getError" appState) error }
            , getResultCmd result
            )


handlePutQuestionnaireCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutQuestionnaireCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState <| Routes.QuestionnairesRoute IndexRoute )

        Err error ->
            ( { model | savingQuestionnaire = ApiError.toActionResult (lg "apiError.questionnaires.putError" appState) error }
            , getResultCmd result
            )
