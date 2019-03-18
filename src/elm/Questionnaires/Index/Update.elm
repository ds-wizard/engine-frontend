module Questionnaires.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Jwt
import Msgs
import Questionnaires.Common.Models exposing (Questionnaire)
import Questionnaires.Index.Models exposing (Model)
import Questionnaires.Index.Msgs exposing (Msg(..))
import Questionnaires.Requests exposing (deleteQuestionnaire, getQuestionnaires)
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

        ShowHideExportQuestionnaire questionnaire ->
            ( { model | questionnaireToBeExported = questionnaire }, Cmd.none )


getQuestionnairesCompleted : Model -> Result Jwt.JwtError (List Questionnaire) -> ( Model, Cmd Msgs.Msg )
getQuestionnairesCompleted model result =
    case result of
        Ok questionnaires ->
            ( { model | questionnaires = Success questionnaires }
            , Cmd.none
            )

        Err error ->
            ( { model | questionnaires = getServerErrorJwt error "Unable to fetch questionnaire list" }
            , getResultCmd result
            )


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
