module Wizard.Questionnaires.Common.View exposing (visibilityBadge)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Shared.Data.Questionnaire.QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Html exposing (emptyNode)
import Wizard.Common.AppState exposing (AppState)


visibilityBadge : AppState -> QuestionnaireVisibility -> Html msg
visibilityBadge appState questionnaireVisibility =
    if appState.config.questionnaire.questionnaireVisibility.enabled then
        case questionnaireVisibility of
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
