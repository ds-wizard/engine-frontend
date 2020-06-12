module Wizard.Questionnaires.Common.DeleteQuestionnaireModal.View exposing (view)

import Html exposing (Html, p, strong, text)
import Shared.Locale exposing (l, lh)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Modal as Modal
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Models exposing (Model)
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Questionnaires.Common.DeleteQuestionnaireModal.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Questionnaires.Common.DeleteQuestionnaireModal.View"


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
