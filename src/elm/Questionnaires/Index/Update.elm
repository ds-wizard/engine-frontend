module Questionnaires.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Questionnaires as QuestionnairesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Msgs
import Questionnaires.Common.Models exposing (Questionnaire)
import Questionnaires.Index.ExportModal.Models exposing (setQuestionnaire)
import Questionnaires.Index.ExportModal.Update as ExportModal
import Questionnaires.Index.Models exposing (Model)
import Questionnaires.Index.Msgs exposing (Msg(..))


fetchData : (Msg -> Msgs.Msg) -> AppState -> Cmd Msgs.Msg
fetchData wrapMsg appState =
    Cmd.map wrapMsg <|
        QuestionnairesApi.getQuestionnaires appState GetQuestionnairesCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetQuestionnairesCompleted result ->
            getQuestionnairesCompleted model result

        ShowHideDeleteQuestionnaire questionnaire ->
            ( { model | questionnaireToBeDeleted = questionnaire, deletingQuestionnaire = Unset }, Cmd.none )

        DeleteQuestionnaire ->
            handleDeleteQuestionnaire wrapMsg appState model

        DeleteQuestionnaireCompleted result ->
            deleteQuestionnaireCompleted wrapMsg appState model result

        ShowExportQuestionnaire questionnaire ->
            ( { model | exportModalModel = setQuestionnaire questionnaire model.exportModalModel }
            , Cmd.map (wrapMsg << ExportModalMsg) <| ExportModal.fetchData appState
            )

        ExportModalMsg exportModalMsg ->
            let
                ( exportModalModel, cmd ) =
                    ExportModal.update exportModalMsg (wrapMsg << ExportModalMsg) appState model.exportModalModel
            in
            ( { model | exportModalModel = exportModalModel }, cmd )


getQuestionnairesCompleted : Model -> Result ApiError (List Questionnaire) -> ( Model, Cmd Msgs.Msg )
getQuestionnairesCompleted model result =
    case result of
        Ok questionnaires ->
            ( { model | questionnaires = Success questionnaires }
            , Cmd.none
            )

        Err error ->
            ( { model | questionnaires = getServerError error "Unable to get questionnaires." }
            , getResultCmd result
            )


handleDeleteQuestionnaire : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteQuestionnaire wrapMsg appState model =
    case model.questionnaireToBeDeleted of
        Just questionnaire ->
            let
                newModel =
                    { model | deletingQuestionnaire = Loading }

                cmd =
                    Cmd.map wrapMsg <|
                        QuestionnairesApi.deleteQuestionnaire questionnaire.uuid appState DeleteQuestionnaireCompleted
            in
            ( newModel, cmd )

        _ ->
            ( model, Cmd.none )


deleteQuestionnaireCompleted : (Msg -> Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
deleteQuestionnaireCompleted wrapMsg appState model result =
    case result of
        Ok user ->
            ( { model | deletingQuestionnaire = Success "Questionnaire was sucessfully deleted", questionnaires = Loading, questionnaireToBeDeleted = Nothing }
            , fetchData wrapMsg appState
            )

        Err error ->
            ( { model | deletingQuestionnaire = getServerError error "Questionnaire could not be deleted" }
            , getResultCmd result
            )
