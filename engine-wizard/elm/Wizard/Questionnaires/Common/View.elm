module Wizard.Questionnaires.Common.View exposing (accessibilityBadge)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Shared.Html exposing (emptyNode)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Questionnaires.Common.QuestionnaireAccessibility exposing (QuestionnaireAccessibility(..))


accessibilityBadge : AppState -> QuestionnaireAccessibility -> Html msg
accessibilityBadge appState questionnaireAccessibility =
    if appState.config.questionnaires.questionnaireAccessibility.enabled then
        case questionnaireAccessibility of
            PublicQuestionnaire ->
                span [ class "badge badge-cyan" ]
                    [ text "public" ]

            PublicReadOnlyQuestionnaire ->
                span [ class "badge badge-purple" ]
                    [ text "read-only" ]

            PrivateQuestionnaire ->
                span [ class "badge badge-red" ]
                    [ text "private" ]

    else
        emptyNode
