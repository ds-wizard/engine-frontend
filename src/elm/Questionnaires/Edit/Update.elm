module Questionnaires.Edit.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import Form
import Jwt
import Models exposing (State)
import Msgs
import Questionnaires.Edit.Models exposing (Model, QuestionnaireEditForm, encodeEditForm, initQuestionnaireEditForm, questionnaireEditFormValidation)
import Questionnaires.Edit.Msgs exposing (Msg(..))
import Questionnaires.Requests exposing (getQuestionnaire, putQuestionnaire)
import Questionnaires.Routing exposing (Route(..))
import Requests exposing (getResultCmd)
import Routing exposing (cmdNavigate)


fetchData : (Msg -> Msgs.Msg) -> Session -> String -> Cmd Msgs.Msg
fetchData wrapMsg session uuid =
    getQuestionnaire uuid session
        |> Jwt.send GetQuestionnaireCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        FormMsg formMsg ->
            handleForm formMsg wrapMsg state.session model

        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted result model

        PutQuestionnaireCompleted result ->
            handlePutQuestionnaireCompleted result state model


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg session model =
    case ( formMsg, Form.getOutput model.editForm, model.questionnaire ) of
        ( Form.Submit, Just editForm, Success questionnaire ) ->
            ( { model | savingQuestionnaire = Loading }
            , putQuestionnaireCmd wrapMsg session model.uuid questionnaire editForm
            )

        _ ->
            let
                editForm =
                    Form.update questionnaireEditFormValidation formMsg model.editForm
            in
            ( { model | editForm = editForm }, Cmd.none )


handleGetQuestionnaireCompleted : Result Jwt.JwtError QuestionnaireDetail -> Model -> ( Model, Cmd Msgs.Msg )
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
            ( { model | questionnaire = getServerErrorJwt error "Unable to get questionnaire detail." }
            , getResultCmd result
            )


handlePutQuestionnaireCompleted : Result Jwt.JwtError String -> State -> Model -> ( Model, Cmd Msgs.Msg )
handlePutQuestionnaireCompleted result state model =
    case result of
        Ok _ ->
            ( model, cmdNavigate state.key <| Routing.Questionnaires Index )

        Err error ->
            ( { model | savingQuestionnaire = getServerErrorJwt error "Questionnaire could not be saved." }
            , getResultCmd result
            )


putQuestionnaireCmd : (Msg -> Msgs.Msg) -> Session -> String -> QuestionnaireDetail -> QuestionnaireEditForm -> Cmd Msgs.Msg
putQuestionnaireCmd wrapMsg session uuid questionnaire form =
    encodeEditForm questionnaire form
        |> putQuestionnaire uuid session
        |> Jwt.send PutQuestionnaireCompleted
        |> Cmd.map wrapMsg
