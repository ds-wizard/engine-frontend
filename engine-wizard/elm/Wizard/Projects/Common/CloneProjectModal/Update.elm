module Wizard.Projects.Common.CloneProjectModal.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Projects.Common.CloneProjectModal.Models exposing (Model)
import Wizard.Projects.Common.CloneProjectModal.Msgs exposing (Msg(..))
import Wizard.Projects.Common.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)


type alias UpdateConfig =
    { wrapMsg : Msg -> Wizard.Msgs.Msg
    , cloneCompleteCmd : Questionnaire -> Cmd Wizard.Msgs.Msg
    }


update : UpdateConfig -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update cfg msg appState model =
    case msg of
        ShowHideCloneQuestionnaire mbQuestionnaire ->
            handleShowHideDeleteQuestionnaire model mbQuestionnaire

        CloneQuestionnaire ->
            handleDeleteQuestionnaire cfg appState model

        CloneQuestionnaireCompleted result ->
            handleDeleteQuestionnaireCompleted cfg appState model result



-- Handlers


handleShowHideDeleteQuestionnaire : Model -> Maybe QuestionnaireDescriptor -> ( Model, Cmd Wizard.Msgs.Msg )
handleShowHideDeleteQuestionnaire model mbQuestionnaire =
    ( { model | questionnaireToBeDeleted = mbQuestionnaire, cloningQuestionnaire = Unset }
    , Cmd.none
    )


handleDeleteQuestionnaire : UpdateConfig -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteQuestionnaire cfg appState model =
    case model.questionnaireToBeDeleted of
        Just questionnaire ->
            let
                newModel =
                    { model | cloningQuestionnaire = Loading }

                cmd =
                    Cmd.map cfg.wrapMsg <|
                        QuestionnairesApi.cloneQuestionnaire questionnaire.uuid appState CloneQuestionnaireCompleted
            in
            ( newModel, cmd )

        _ ->
            ( model, Cmd.none )


handleDeleteQuestionnaireCompleted : UpdateConfig -> AppState -> Model -> Result ApiError Questionnaire -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteQuestionnaireCompleted cfg appState model result =
    case result of
        Ok questionnaire ->
            ( { model
                | cloningQuestionnaire = Success <| lg "apiSuccess.questionnaires.clone" appState
                , questionnaireToBeDeleted = Nothing
              }
            , cfg.cloneCompleteCmd questionnaire
            )

        Err error ->
            ( { model | cloningQuestionnaire = ApiError.toActionResult (lg "apiError.questionnaires.cloneError" appState) error }
            , Cmd.none
            )
