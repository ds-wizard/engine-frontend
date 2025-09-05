module Wizard.Pages.Projects.Common.DeleteProjectModal.Update exposing (UpdateConfig, update)

import ActionResult exposing (ActionResult(..))
import Common.Data.ApiError as ApiError exposing (ApiError)
import Gettext exposing (gettext)
import Wizard.Api.Questionnaires as QuestionnairesApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Projects.Common.DeleteProjectModal.Models exposing (Model)
import Wizard.Pages.Projects.Common.DeleteProjectModal.Msgs exposing (Msg(..))
import Wizard.Pages.Projects.Common.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)


type alias UpdateConfig =
    { wrapMsg : Msg -> Wizard.Msgs.Msg
    , deleteCompleteCmd : Cmd Wizard.Msgs.Msg
    }


update : UpdateConfig -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update cfg msg appState model =
    case msg of
        ShowHideDeleteQuestionnaire mbQuestionnaire ->
            handleShowHideDeleteQuestionnaire model mbQuestionnaire

        DeleteQuestionnaire ->
            handleDeleteQuestionnaire cfg appState model

        DeleteQuestionnaireCompleted result ->
            handleDeleteQuestionnaireCompleted cfg appState model result



-- Handlers


handleShowHideDeleteQuestionnaire : Model -> Maybe QuestionnaireDescriptor -> ( Model, Cmd Wizard.Msgs.Msg )
handleShowHideDeleteQuestionnaire model mbQuestionnaire =
    ( { model | questionnaireToBeDeleted = mbQuestionnaire, deletingQuestionnaire = Unset }
    , Cmd.none
    )


handleDeleteQuestionnaire : UpdateConfig -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteQuestionnaire cfg appState model =
    case model.questionnaireToBeDeleted of
        Just questionnaire ->
            let
                newModel =
                    { model | deletingQuestionnaire = Loading }

                cmd =
                    Cmd.map cfg.wrapMsg <|
                        QuestionnairesApi.deleteQuestionnaire appState questionnaire.uuid DeleteQuestionnaireCompleted
            in
            ( newModel, cmd )

        _ ->
            ( model, Cmd.none )


handleDeleteQuestionnaireCompleted : UpdateConfig -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteQuestionnaireCompleted cfg appState model result =
    case result of
        Ok _ ->
            ( { model
                | deletingQuestionnaire = Unset
                , questionnaireToBeDeleted = Nothing
              }
            , cfg.deleteCompleteCmd
            )

        Err error ->
            ( { model | deletingQuestionnaire = ApiError.toActionResult appState (gettext "Project could not be deleted." appState.locale) error }
            , Cmd.none
            )
