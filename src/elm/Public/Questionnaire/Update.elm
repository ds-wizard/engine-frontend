module Public.Questionnaire.Update exposing (..)

import Common.Models exposing (getServerError)
import Common.Questionnaire.Models exposing (QuestionnaireDetail, initialModel)
import Common.Questionnaire.Msgs
import Common.Questionnaire.Update
import Common.Types exposing (ActionResult(Success))
import Http
import Msgs
import Public.Questionnaire.Models exposing (Model)
import Public.Questionnaire.Msgs exposing (Msg(..))
import Public.Questionnaire.Requests exposing (getQuestionnaire)


fetchData : (Msg -> Msgs.Msg) -> Cmd Msgs.Msg
fetchData wrapMsg =
    getQuestionnaire
        |> Http.send GetQuestionnaireCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg model =
    case msg of
        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted model result

        QuestionnaireMsg msg ->
            handleQuestionnaireMsg msg model


handleGetQuestionnaireCompleted : Model -> Result Http.Error QuestionnaireDetail -> ( Model, Cmd Msgs.Msg )
handleGetQuestionnaireCompleted model result =
    let
        newModel =
            case result of
                Ok questionnaireDetail ->
                    { model | questionnaireModel = Success <| initialModel questionnaireDetail }

                Err error ->
                    { model | questionnaireModel = getServerError error "Unable to get questionnaire." }
    in
    ( newModel, Cmd.none )


handleQuestionnaireMsg : Common.Questionnaire.Msgs.Msg -> Model -> ( Model, Cmd Msgs.Msg )
handleQuestionnaireMsg msg model =
    let
        newQuestionnaireModel =
            case model.questionnaireModel of
                Success questionnaireModel ->
                    Success <| Common.Questionnaire.Update.update msg questionnaireModel

                _ ->
                    model.questionnaireModel
    in
    ( { model | questionnaireModel = newQuestionnaireModel }, Cmd.none )
