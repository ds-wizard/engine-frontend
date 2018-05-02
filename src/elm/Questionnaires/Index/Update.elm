module Questionnaires.Index.Update exposing (..)

import Auth.Models exposing (Session)
import Common.Types exposing (ActionResult(..))
import Jwt
import Msgs
import Questionnaires.Common.Models exposing (Questionnaire)
import Questionnaires.Index.Models exposing (Model)
import Questionnaires.Index.Msgs exposing (Msg(..))
import Questionnaires.Requests exposing (deleteQuestionnaire, getQuestionnaires)


fetchData : (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchData wrapMsg session =
    getQuestionnaires session
        |> Jwt.send GetQuestionnairesCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg session model =
    case msg of
        GetQuestionnairesCompleted result ->
            getQuestionnairesCompleted model result

        ShowHideDeleteQuestionnaire questionnaire ->
            ( { model | questionnaireToBeDeleted = questionnaire, deletingQuestionnaire = Unset }, Cmd.none )

        DeleteQuestionnaire ->
            handleDeleteQuestionnaire wrapMsg session model

        DeleteQuestionnaireCompleted result ->
            deleteQuestionnaireCompleted wrapMsg session model result


getQuestionnairesCompleted : Model -> Result Jwt.JwtError (List Questionnaire) -> ( Model, Cmd Msgs.Msg )
getQuestionnairesCompleted model result =
    let
        newModel =
            case result of
                Ok users ->
                    { model | questionnaires = Success users }

                Err error ->
                    { model | questionnaires = Error "Unable to fetch questionnaire list" }
    in
    ( newModel, Cmd.none )


handleDeleteQuestionnaire : (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteQuestionnaire wrapMsg session model =
    case model.questionnaireToBeDeleted of
        Just questionnaire ->
            let
                newModel =
                    { model | deletingQuestionnaire = Loading }

                cmd =
                    deleteQuestionnaire questionnaire.uuid session
                        |> Jwt.send DeleteQuestionnaireCompleted
                        |> Cmd.map wrapMsg
            in
            ( newModel, cmd )

        _ ->
            ( model, Cmd.none )


deleteQuestionnaireCompleted : (Msg -> Msgs.Msg) -> Session -> Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
deleteQuestionnaireCompleted wrapMsg session model result =
    case result of
        Ok user ->
            ( { model | deletingQuestionnaire = Success "Questionnaire was sucessfully deleted", questionnaires = Loading, questionnaireToBeDeleted = Nothing }
            , fetchData wrapMsg session
            )

        Err error ->
            ( { model | deletingQuestionnaire = Error "Questionnaire could not be deleted" }
            , Cmd.none
            )
