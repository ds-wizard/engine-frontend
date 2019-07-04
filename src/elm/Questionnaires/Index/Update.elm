module Questionnaires.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (applyResult, getResultCmd)
import Common.Api.Packages as PackagesApi
import Common.Api.Questionnaires as QuestionnairesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Setters exposing (setQuestionnaires)
import Form
import KnowledgeModels.Common.PackageDetail exposing (PackageDetail)
import Msgs
import Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Questionnaires.Common.QuestionnaireMigration exposing (QuestionnaireMigration)
import Questionnaires.Index.ExportModal.Models exposing (setQuestionnaire)
import Questionnaires.Index.ExportModal.Update as ExportModal
import Questionnaires.Index.Models exposing (Model)
import Questionnaires.Index.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))
import Routing exposing (cmdNavigate)


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

        DeleteQuestionnaireMigration uuid ->
            handleDeleteMigration wrapMsg appState uuid model

        DeleteQuestionnaireMigrationCompleted result ->
            deleteMigrationCompleted wrapMsg appState model result


getQuestionnairesCompleted : Model -> Result ApiError (List Questionnaire) -> ( Model, Cmd Msgs.Msg )
getQuestionnairesCompleted model result =
    applyResult
        { setResult = setQuestionnaires
        , defaultError = "Unable to get questionnaires."
        , model = model
        , result = result
        }


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


handleDeleteMigration : (Msg -> Msgs.Msg) -> AppState -> String -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteMigration wrapMsg appState uuid model =
    ( { model | deletingMigration = Loading }, deletingMigrationCmd wrapMsg appState uuid )


deleteMigrationCompleted : (Msg -> Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
deleteMigrationCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            ( { model | deletingMigration = Success "Questionnaire migration was canceled.", questionnaires = Loading }
            , fetchData wrapMsg appState
            )

        Err error ->
            ( { model | deletingMigration = getServerError error "Questionnaire migration could not be canceled." }
            , getResultCmd result
            )


deletingMigrationCmd : (Msg -> Msgs.Msg) -> AppState -> String -> Cmd Msgs.Msg
deletingMigrationCmd wrapMsg appState uuid =
    Cmd.map wrapMsg <|
        QuestionnairesApi.deleteQuestionnaireMigration uuid appState DeleteQuestionnaireMigrationCompleted
