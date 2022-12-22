module Wizard.Projects.Common.CloneProjectModal.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, i, p, strong, text)
import Html.Attributes exposing (class)
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Modal as Modal
import Wizard.Projects.Common.CloneProjectModal.Models exposing (Model)
import Wizard.Projects.Common.CloneProjectModal.Msgs exposing (Msg(..))


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
                    (gettext "Do you want to create a copy of %s?" appState.locale)
                    [ strong [] [ text name ] ]
                )
            , p [ class "text-muted" ]
                [ i [] [ text (gettext "The original project will remain unchanged." appState.locale) ] ]
            ]

        modalConfig =
            { modalTitle = gettext "Clone Project" appState.locale
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.cloningQuestionnaire
            , actionName = gettext "Clone" appState.locale
            , actionMsg = CloneQuestionnaire
            , cancelMsg = Just <| ShowHideCloneQuestionnaire Nothing
            , dangerous = False
            , dataCy = "clone-project"
            }
    in
    Modal.confirm appState modalConfig
