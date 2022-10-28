module Wizard.Projects.Common.DeleteProjectModal.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, p, strong, text)
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Modal as Modal
import Wizard.Projects.Common.DeleteProjectModal.Models exposing (Model)
import Wizard.Projects.Common.DeleteProjectModal.Msgs exposing (Msg(..))


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
            { modalTitle = gettext "Delete Project" appState.locale
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingQuestionnaire
            , actionName = gettext "Delete" appState.locale
            , actionMsg = DeleteQuestionnaire
            , cancelMsg = Just <| ShowHideDeleteQuestionnaire Nothing
            , dangerous = True
            , dataCy = "project-delete"
            }
    in
    Modal.confirm appState modalConfig
