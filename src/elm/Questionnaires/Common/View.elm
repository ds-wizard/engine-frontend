module Questionnaires.Common.View exposing (accessibilityBadge)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Questionnaires.Common.Models.QuestionnaireAccessibility exposing (QuestionnaireAccessibility(..))


accessibilityBadge : QuestionnaireAccessibility -> Html msg
accessibilityBadge questionnaireAccessibility =
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
