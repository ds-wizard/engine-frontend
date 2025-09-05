module Wizard.Pages.Projects.Common.CloneProjectModal.Update exposing (UpdateConfig, update)

import ActionResult exposing (ActionResult(..))
import Common.Data.ApiError as ApiError exposing (ApiError)
import Gettext exposing (gettext)
import Wizard.Api.Models.Questionnaire exposing (Questionnaire)
import Wizard.Api.Questionnaires as QuestionnairesApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Projects.Common.CloneProjectModal.Models exposing (Model)
import Wizard.Pages.Projects.Common.CloneProjectModal.Msgs exposing (Msg(..))
import Wizard.Pages.Projects.Common.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)


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
                        QuestionnairesApi.cloneQuestionnaire appState questionnaire.uuid CloneQuestionnaireCompleted
            in
            ( newModel, cmd )

        _ ->
            ( model, Cmd.none )


handleDeleteQuestionnaireCompleted : UpdateConfig -> AppState -> Model -> Result ApiError Questionnaire -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteQuestionnaireCompleted cfg appState model result =
    case result of
        Ok questionnaire ->
            ( { model
                | cloningQuestionnaire = Success <| gettext "%s has been created." appState.locale
                , questionnaireToBeDeleted = Nothing
              }
            , cfg.cloneCompleteCmd questionnaire
            )

        Err error ->
            ( { model | cloningQuestionnaire = ApiError.toActionResult appState (gettext "Unable to clone project." appState.locale) error }
            , Cmd.none
            )
