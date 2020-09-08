module Wizard.Projects.Common.View exposing (visibilityIcons)

import Html exposing (Html, i)
import Html.Attributes exposing (class, title)
import Shared.Data.Questionnaire.QuestionnaireSharing exposing (QuestionnaireSharing(..))
import Shared.Data.Questionnaire.QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Locale exposing (l)
import Shared.Utils exposing (listInsertIf)
import Wizard.Common.AppState exposing (AppState)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Common.View"


visibilityIcons : AppState -> { q | visibility : QuestionnaireVisibility, sharing : QuestionnaireSharing } -> List (Html msg)
visibilityIcons appState questionnaire =
    let
        visibleIcon =
            i [ class "fa fas fa-user-friends", title visibleTitle ] []

        visibleTitle =
            if questionnaire.visibility == VisibleEditQuestionnaire then
                l_ "visibilityIcons.titleLoggedEdit" appState

            else
                l_ "visibilityIcons.titleLoggedView" appState

        linkIcon =
            i [ class "fa fas fa-link", title linkTitle ] []

        linkTitle =
            if questionnaire.sharing == AnyoneWithLinkEditQuestionnaire then
                l_ "visibilityIcons.titleAnyoneEdit" appState

            else
                l_ "visibilityIcons.titleAnyoneView" appState
    in
    []
        |> listInsertIf visibleIcon (questionnaire.visibility /= PrivateQuestionnaire)
        |> listInsertIf linkIcon (questionnaire.sharing /= RestrictedQuestionnaire)
