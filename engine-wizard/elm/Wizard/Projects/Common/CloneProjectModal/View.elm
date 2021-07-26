module Wizard.Projects.Common.CloneProjectModal.View exposing (view)

import Html exposing (Html, i, p, strong, text)
import Html.Attributes exposing (class)
import Shared.Locale exposing (l, lh, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Modal as Modal
import Wizard.Projects.Common.CloneProjectModal.Models exposing (Model)
import Wizard.Projects.Common.CloneProjectModal.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Common.CloneProjectModal.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Projects.Common.CloneProjectModal.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Projects.Common.CloneProjectModal.View"


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
                (lh_ "cloneModal.message" [ strong [] [ text name ] ] appState)
            , p [ class "text-muted" ]
                [ i [] [ lx_ "cloneModal.info" appState ] ]
            ]

        modalConfig =
            { modalTitle = l_ "cloneModal.title" appState
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.cloningQuestionnaire
            , actionName = l_ "cloneModal.action" appState
            , actionMsg = CloneQuestionnaire
            , cancelMsg = Just <| ShowHideCloneQuestionnaire Nothing
            , dangerous = False
            , dataCy = "clone-project"
            }
    in
    Modal.confirm appState modalConfig
