module Questionnaires.Detail.Update exposing (..)

import Auth.Models exposing (Session)
import Common.Types exposing (ActionResult(..))
import Jwt
import Msgs
import Questionnaires.Common.Models exposing (QuestionnaireDetail)
import Questionnaires.Detail.Models exposing (Model)
import Questionnaires.Detail.Msgs exposing (Msg(..))
import Questionnaires.Requests exposing (getQuestionnaire)


fetchData : (Msg -> Msgs.Msg) -> Session -> String -> Cmd Msgs.Msg
fetchData wrapMsg session uuid =
    getQuestionnaire uuid session
        |> Jwt.send GetQuestionnaireCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg model =
    case msg of
        GetQuestionnaireCompleted result ->
            getQuestionnaireCompleted model result


getQuestionnaireCompleted : Model -> Result Jwt.JwtError QuestionnaireDetail -> ( Model, Cmd Msgs.Msg )
getQuestionnaireCompleted model result =
    let
        questionnaire =
            case result of
                Ok questionnaireDetail ->
                    Success questionnaireDetail

                Err error ->
                    Error "Unable to get questionnaire."
    in
    ( { model | questionnaire = questionnaire }, Cmd.none )
