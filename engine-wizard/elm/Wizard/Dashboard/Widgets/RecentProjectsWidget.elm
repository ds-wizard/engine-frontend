module Wizard.Dashboard.Widgets.RecentProjectsWidget exposing (view)

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Html exposing (Html, br, div, h2, p, strong, text)
import Html.Attributes exposing (class, style)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Html exposing (faSet)
import String.Format as String
import Time.Distance exposing (inWordsWithConfig)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.TimeDistance exposing (locale)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


view : AppState -> ActionResult (List Questionnaire) -> Html msg
view appState questionnaires =
    WidgetHelpers.widget <|
        case questionnaires of
            Unset ->
                []

            Loading ->
                [ WidgetHelpers.widgetLoader appState ]

            Error error ->
                [ WidgetHelpers.widgetError appState error ]

            Success questionnaireList ->
                if List.isEmpty questionnaireList then
                    viewRecentProjectsEmpty appState

                else
                    viewRecentProjects appState questionnaireList


viewRecentProjects : AppState -> List Questionnaire -> List (Html msg)
viewRecentProjects appState questionnaires =
    [ div [ class "RecentProjectsWidget d-flex flex-column h-100" ]
        [ h2 [ class "fs-4 fw-bold mb-4" ] [ text (gettext "Recent Projects" appState.locale) ]
        , div [ class "Dashboard__ItemList flex-grow-1" ] (List.map (viewProject appState) questionnaires)
        , div [ class "mt-4" ]
            [ linkTo appState (Routes.projectsIndex appState) [] [ text (gettext "View all" appState.locale) ] ]
        ]
    ]


viewProject : AppState -> Questionnaire -> Html msg
viewProject appState questionnaire =
    let
        projectProgressView =
            let
                pctg =
                    (toFloat questionnaire.answeredQuestions / toFloat (questionnaire.answeredQuestions + questionnaire.unansweredQuestions)) * 100
            in
            div [ class "progress mt-1 flex-grow-1", style "height" "7px" ]
                [ div [ class "progress-bar bg-info", style "width" (String.fromFloat pctg ++ "%") ] [] ]

        updatedText =
            inWordsWithConfig { withAffix = True } (locale appState) questionnaire.updatedAt appState.currentTime
    in
    linkTo appState
        (Routes.projectsDetailQuestionnaire questionnaire.uuid Nothing)
        [ class "p-2 py-3 d-flex rounded-3" ]
        [ ItemIcon.view { text = questionnaire.name, image = Nothing }
        , div [ class "ms-2 flex-grow-1 content" ]
            [ strong [] [ text questionnaire.name ]
            , div [ class "d-flex align-items-center" ]
                [ projectProgressView
                , div [ class "flex-grow-1 ps-4 text-lighter fst-italic" ] [ text (String.format (gettext "Updated %s" appState.locale) [ updatedText ]) ]
                ]
            ]
        ]


viewRecentProjectsEmpty : AppState -> List (Html msg)
viewRecentProjectsEmpty appState =
    [ div [ class "text-lighter d-flex flex-column justify-content-center h-100" ]
        [ p [ class "fs-5 m-0 mt-3 text-center" ]
            [ text (gettext "You have no projects yet, start by creating some." appState.locale)
            , br [] []
            , faSet "_global.arrowRight" appState
            ]
        ]
    ]
