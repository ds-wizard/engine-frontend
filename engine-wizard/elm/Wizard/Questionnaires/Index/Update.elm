module Wizard.Questionnaires.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.Api.Questionnaires as QuestionnairesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Locale exposing (lg)
import Wizard.Common.Setters exposing (setQuestionnaires)
import Wizard.Msgs
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Wizard.Questionnaires.Index.ExportModal.Models exposing (setQuestionnaire)
import Wizard.Questionnaires.Index.ExportModal.Msgs as ExportModal
import Wizard.Questionnaires.Index.ExportModal.Update as ExportModal
import Wizard.Questionnaires.Index.Models exposing (Model)
import Wizard.Questionnaires.Index.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    QuestionnairesApi.getQuestionnaires appState GetQuestionnairesCompleted


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GetQuestionnairesCompleted result ->
            handleGetQuestionnairesCompleted appState model result

        ShowHideDeleteQuestionnaire mbQuestionnaire ->
            handleShowHideDeleteQuestionnaire model mbQuestionnaire

        DeleteQuestionnaire ->
            handleDeleteQuestionnaire wrapMsg appState model

        DeleteQuestionnaireCompleted result ->
            handleDeleteQuestionnaireCompleted wrapMsg appState model result

        ShowExportQuestionnaire questionnaire ->
            handleShowExportQuestionnaire wrapMsg appState model questionnaire

        ExportModalMsg exportModalMsg ->
            handleExportModal exportModalMsg appState model

        DeleteQuestionnaireMigration uuid ->
            handleDeleteMigration wrapMsg appState model uuid

        DeleteQuestionnaireMigrationCompleted result ->
            handleDeleteMigrationCompleted wrapMsg appState model result



-- Handlers


handleGetQuestionnairesCompleted : AppState -> Model -> Result ApiError (List Questionnaire) -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetQuestionnairesCompleted appState model result =
    applyResult
        { setResult = setQuestionnaires
        , defaultError = lg "apiError.questionnaires.getListError" appState
        , model = model
        , result = result
        }


handleShowHideDeleteQuestionnaire : Model -> Maybe Questionnaire -> ( Model, Cmd Wizard.Msgs.Msg )
handleShowHideDeleteQuestionnaire model mbQuestionnaire =
    ( { model | questionnaireToBeDeleted = mbQuestionnaire, deletingQuestionnaire = Unset }
    , Cmd.none
    )


handleDeleteQuestionnaire : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
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


handleDeleteQuestionnaireCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteQuestionnaireCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            ( { model | deletingQuestionnaire = Success <| lg "apiSuccess.questionnaires.delete" appState, questionnaires = Loading, questionnaireToBeDeleted = Nothing }
            , Cmd.map wrapMsg <| fetchData appState
            )

        Err error ->
            ( { model | deletingQuestionnaire = ApiError.toActionResult (lg "apiError.questionnaires.deleteError" appState) error }
            , getResultCmd result
            )


handleShowExportQuestionnaire : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Questionnaire -> ( Model, Cmd Wizard.Msgs.Msg )
handleShowExportQuestionnaire wrapMsg appState model questionnaire =
    ( { model | exportModalModel = setQuestionnaire questionnaire model.exportModalModel }
    , Cmd.map (wrapMsg << ExportModalMsg) <| ExportModal.fetchData appState
    )


handleExportModal : ExportModal.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleExportModal exportModalMsg appState model =
    let
        ( exportModalModel, cmd ) =
            ExportModal.update exportModalMsg appState model.exportModalModel
    in
    ( { model | exportModalModel = exportModalModel }, cmd )


handleDeleteMigration : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> String -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteMigration wrapMsg appState model uuid =
    ( { model | deletingMigration = Loading }
    , QuestionnairesApi.deleteQuestionnaireMigration uuid appState (wrapMsg << DeleteQuestionnaireMigrationCompleted)
    )


handleDeleteMigrationCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteMigrationCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            ( { model | deletingMigration = Success <| lg "apiSuccess.questionnaires.migration.delete" appState, questionnaires = Loading }
            , Cmd.map wrapMsg <| fetchData appState
            )

        Err error ->
            ( { model | deletingMigration = ApiError.toActionResult (lg "apiError.questionnaires.migrations.deleteError" appState) error }
            , getResultCmd result
            )
