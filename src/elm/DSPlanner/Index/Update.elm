module DSPlanner.Index.Update exposing (..)

import Auth.Models exposing (Session)
import Bootstrap.Dropdown as Dropdown
import Common.Models exposing (getServerErrorJwt)
import Common.Types exposing (ActionResult(..))
import DSPlanner.Common.Models exposing (Questionnaire)
import DSPlanner.Index.Models exposing (Model, QuestionnaireRow, initQuestionnaireRow)
import DSPlanner.Index.Msgs exposing (Msg(..))
import DSPlanner.Requests exposing (deleteQuestionnaire, getQuestionnaires)
import Jwt
import Msgs
import Requests exposing (getResultCmd)


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

        DropdownMsg questionnaire state ->
            handleDropdownToggle model questionnaire state


getQuestionnairesCompleted : Model -> Result Jwt.JwtError (List Questionnaire) -> ( Model, Cmd Msgs.Msg )
getQuestionnairesCompleted model result =
    let
        newModel =
            case result of
                Ok questionnaires ->
                    { model | questionnaires = Success <| List.map initQuestionnaireRow questionnaires }

                Err error ->
                    { model | questionnaires = getServerErrorJwt error "Unable to fetch questionnaire list" }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


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
            ( { model | deletingQuestionnaire = getServerErrorJwt error "Questionnaire could not be deleted" }
            , getResultCmd result
            )


handleDropdownToggle : Model -> Questionnaire -> Dropdown.State -> ( Model, Cmd Msgs.Msg )
handleDropdownToggle model questionnaire state =
    case model.questionnaires of
        Success questionnaireRows ->
            let
                replaceWith row =
                    if row.questionnaire == questionnaire then
                        { row | dropdownState = state }
                    else
                        row

                newRows =
                    List.map replaceWith questionnaireRows
            in
            ( { model | questionnaires = Success newRows }, Cmd.none )

        _ ->
            ( model, Cmd.none )
