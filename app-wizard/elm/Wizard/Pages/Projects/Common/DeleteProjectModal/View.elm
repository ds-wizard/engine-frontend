module Wizard.Pages.Projects.Common.DeleteProjectModal.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, p, strong, text)
import Shared.Components.Modal as Modal
import String.Format as String
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Projects.Common.DeleteProjectModal.Models exposing (Model)
import Wizard.Pages.Projects.Common.DeleteProjectModal.Msgs exposing (Msg(..))


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
