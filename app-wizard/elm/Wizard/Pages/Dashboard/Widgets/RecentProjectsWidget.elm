module Wizard.Pages.Dashboard.Widgets.RecentProjectsWidget exposing (view)

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Html exposing (Html, br, div, h2, p, strong, text)
import Html.Attributes exposing (class)
import Shared.Components.FontAwesome exposing (faArrowRight)
import Shared.Utils.TimeDistance exposing (locale)
import String.Format as String
import Time.Distance exposing (inWordsWithConfig)
import Wizard.Api.Models.Questionnaire exposing (Questionnaire)
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.ItemIcon as ItemIcon
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


view : AppState -> ActionResult (List Questionnaire) -> Html msg
view appState questionnaires =
    WidgetHelpers.widget <|
        case questionnaires of
            Unset ->
                []

            Loading ->
                [ WidgetHelpers.widgetLoader ]

            Error error ->
                [ WidgetHelpers.widgetError error ]

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
            [ linkTo (Routes.projectsIndex appState) [] [ text (gettext "View all" appState.locale) ] ]
        ]
    ]


viewProject : AppState -> Questionnaire -> Html msg
viewProject appState questionnaire =
    let
        updatedText =
            inWordsWithConfig { withAffix = True } (locale appState.locale) questionnaire.updatedAt appState.currentTime
    in
    linkTo (Routes.projectsDetail questionnaire.uuid)
        [ class "p-2 py-3 d-flex rounded-3" ]
        [ ItemIcon.view { text = questionnaire.name, image = Nothing }
        , div [ class "ms-2 flex-grow-1 content" ]
            [ strong [] [ text questionnaire.name ]
            , div [ class "d-flex align-items-center" ]
                [ div [ class "flex-grow-1 text-lighter fst-italic" ] [ text (String.format (gettext "Updated %s" appState.locale) [ updatedText ]) ]
                ]
            ]
        ]


viewRecentProjectsEmpty : AppState -> List (Html msg)
viewRecentProjectsEmpty appState =
    [ div [ class "text-lighter d-flex flex-column justify-content-center h-100" ]
        [ p [ class "fs-5 m-0 mt-3 text-center" ]
            [ text (gettext "You have no projects yet, start by creating some." appState.locale)
            , br [] []
            , faArrowRight
            ]
        ]
    ]
