module Wizard.Projects.Detail.Components.Settings.DeleteModal exposing (Model, Msg, UpdateConfig, initialModel, open, update, view)

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, p, strong, text)
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (l, lg, lh)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Modal as Modal
import Wizard.Projects.Common.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Detail.Components.Settings.DeleteModal"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Projects.Detail.Components.Settings.DeleteModal"


type alias Model =
    { questionnaireToBeDeleted : Maybe QuestionnaireDescriptor
    , deletingQuestionnaire : ActionResult String
    }


initialModel : Model
initialModel =
    { questionnaireToBeDeleted = Nothing
    , deletingQuestionnaire = Unset
    }


type Msg
    = ShowHideDeleteQuestionnaire (Maybe QuestionnaireDescriptor)
    | DeleteQuestionnaire
    | DeleteQuestionnaireCompleted (Result ApiError ())


open : QuestionnaireDescriptor -> Msg
open =
    ShowHideDeleteQuestionnaire << Just


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , deleteCompleteCmd : Cmd msg
    }


update : UpdateConfig msg -> Msg -> AppState -> Model -> ( Model, Cmd msg )
update cfg msg appState model =
    case msg of
        ShowHideDeleteQuestionnaire mbQuestionnaire ->
            handleShowHideDeleteQuestionnaire model mbQuestionnaire

        DeleteQuestionnaire ->
            handleDeleteQuestionnaire cfg appState model

        DeleteQuestionnaireCompleted result ->
            handleDeleteQuestionnaireCompleted cfg appState model result


handleShowHideDeleteQuestionnaire : Model -> Maybe QuestionnaireDescriptor -> ( Model, Cmd msg )
handleShowHideDeleteQuestionnaire model mbQuestionnaire =
    ( { model | questionnaireToBeDeleted = mbQuestionnaire, deletingQuestionnaire = Unset }
    , Cmd.none
    )


handleDeleteQuestionnaire : UpdateConfig msg -> AppState -> Model -> ( Model, Cmd msg )
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


handleDeleteQuestionnaireCompleted : UpdateConfig msg -> AppState -> Model -> Result ApiError () -> ( Model, Cmd msg )
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


view : AppState -> Model -> Html Msg
view appState model =
    let
        ( visible, name ) =
            case model.questionnaireToBeDeleted of
                Just questionnaire ->
                    ( True, questionnaire.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                (lh_ "deleteModal.message" [ strong [] [ text name ] ] appState)
            ]

        modalConfig =
            { modalTitle = l_ "deleteModal.title" appState
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingQuestionnaire
            , actionName = l_ "deleteModal.action" appState
            , actionMsg = DeleteQuestionnaire
            , cancelMsg = Just <| ShowHideDeleteQuestionnaire Nothing
            , dangerous = True
            }
    in
    Modal.confirm appState modalConfig
