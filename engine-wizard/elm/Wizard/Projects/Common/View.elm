module Wizard.Projects.Common.View exposing (visibilityIcons)

import Gettext exposing (gettext)
import Html exposing (Html, i, span)
import Html.Attributes exposing (class)
import Shared.Data.Questionnaire.QuestionnaireSharing exposing (QuestionnaireSharing(..))
import Shared.Data.Questionnaire.QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Utils exposing (listInsertIf)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (tooltipRight)


visibilityIcons : AppState -> { q | visibility : QuestionnaireVisibility, sharing : QuestionnaireSharing } -> List (Html msg)
visibilityIcons appState questionnaire =
    let
        visibleIcon =
            span (tooltipRight visibleTitle)
                [ i [ class "fa fas fa-user-friends" ] [] ]

        visibleTitle =
            if questionnaire.visibility == VisibleEditQuestionnaire then
                gettext "Other logged-in users can edit the project." appState.locale

            else
                gettext "Other logged-in users can view the project." appState.locale

        linkIcon =
            span (tooltipRight linkTitle)
                [ i [ class "fa fas fa-link" ] [] ]

        linkTitle =
            if questionnaire.sharing == AnyoneWithLinkEditQuestionnaire then
                gettext "Anyone with the link can edit the project." appState.locale

            else
                gettext "Anyone with the link can view the project." appState.locale
    in
    []
        |> listInsertIf visibleIcon (questionnaire.visibility /= PrivateQuestionnaire)
        |> listInsertIf linkIcon (questionnaire.sharing /= RestrictedQuestionnaire)
