module Questionnaires.Common.View exposing (accessibilityBadge)

import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Questionnaires.Common.Models.QuestionnaireAccessibility exposing (QuestionnaireAccessibility(..))


accessibilityBadge : AppState -> QuestionnaireAccessibility -> Html msg
accessibilityBadge appState questionnaireAccessibility =
    if appState.config.questionnaireAccessibilityEnabled then
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
