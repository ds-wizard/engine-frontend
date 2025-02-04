module Wizard.Projects.Detail.Components.Settings.DeleteModal exposing (Model, Msg, UpdateConfig, initialModel, open, update, view)

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Html exposing (Html, p, strong, text)
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Modal as Modal
import Wizard.Projects.Common.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)


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
                | deletingQuestionnaire = Success <| gettext "Questionnaire was successfully deleted." appState.locale
                , questionnaireToBeDeleted = Nothing
              }
            , cfg.deleteCompleteCmd
            )

        Err error ->
            ( { model | deletingQuestionnaire = ApiError.toActionResult appState (gettext "Questionnaire could not be deleted." appState.locale) error }
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
                (String.formatHtml
                    (gettext "Are you sure you want to permanently delete %s?" appState.locale)
                    [ strong [] [ text name ] ]
                )
            ]

        modalConfig =
            Modal.confirmConfig (gettext "Delete Project" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible visible
                |> Modal.confirmConfigActionResult model.deletingQuestionnaire
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) DeleteQuestionnaire
                |> Modal.confirmConfigCancelMsg (ShowHideDeleteQuestionnaire Nothing)
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "project-delete"
    in
    Modal.confirm appState modalConfig
