module Wizard.Projects.Common.DeleteProjectModal.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Projects.Common.DeleteProjectModal.Models exposing (Model)
import Wizard.Projects.Common.DeleteProjectModal.Msgs exposing (Msg(..))
import Wizard.Projects.Common.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)


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
                        QuestionnairesApi.deleteQuestionnaire questionnaire.uuid appState DeleteQuestionnaireCompleted
            in
            ( newModel, cmd )

        _ ->
            ( model, Cmd.none )


handleDeleteQuestionnaireCompleted : UpdateConfig -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteQuestionnaireCompleted cfg appState model result =
    case result of
        Ok _ ->
            ( { model
                | deletingQuestionnaire = Success <| lg "apiSuccess.questionnaires.delete" appState
                , questionnaireToBeDeleted = Nothing
              }
            , cfg.deleteCompleteCmd
            )

        Err error ->
            ( { model | deletingQuestionnaire = ApiError.toActionResult appState (lg "apiError.questionnaires.deleteError" appState) error }
            , Cmd.none
            )
