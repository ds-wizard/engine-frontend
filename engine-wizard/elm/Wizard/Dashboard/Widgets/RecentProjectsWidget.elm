module Wizard.Dashboard.Widgets.RecentProjectsWidget exposing (view)

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, br, div, h2, p, strong, text)
import Html.Attributes exposing (class, style)
import Maybe.Extra as Maybe
import Shared.Data.Questionnaire as Questionnaire exposing (Questionnaire)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (lx)
import Time.Distance exposing (inWordsWithConfig)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.TimeDistance exposing (locale)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Dashboard.Widgets.RecentProjectsWidget"


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
        [ h2 [ class "fs-4 fw-bold mb-4" ] [ lx_ "heading" appState ]
        , div [ class "RecentProjectsWidget__ProjectList flex-grow-1" ] (List.map (viewProject appState) questionnaires)
        , div [ class "mt-4" ]
            [ linkTo appState (Routes.projectsIndex appState) [] [ lx_ "viewAll" appState ] ]
        ]
    ]


viewProject : AppState -> Questionnaire -> Html msg
viewProject appState questionnaire =
    let
        projectProgress ( answered, unanswered ) =
            let
                pctg =
                    (toFloat answered / toFloat (answered + unanswered)) * 100
            in
            div [ class "progress mt-1 flex-grow-1", style "height" "7px" ]
                [ div [ class "progress-bar bg-info", style "width" (String.fromFloat pctg ++ "%") ] [] ]

        projectProgressView =
            Questionnaire.getAnsweredIndication questionnaire
                |> Maybe.unwrap emptyNode projectProgress

        updatedText =
            inWordsWithConfig { withAffix = True } (locale appState) questionnaire.updatedAt appState.currentTime
    in
    linkTo appState
        (Routes.projectsDetailQuestionnaire questionnaire.uuid)
        [ class "p-2 py-3 d-flex rounded-3" ]
        [ ItemIcon.view { text = questionnaire.name, image = Nothing }
        , div [ class "ms-2 flex-grow-1 content" ]
            [ strong [] [ text questionnaire.name ]
            , div [ class "d-flex align-items-center" ]
                [ projectProgressView
                , div [ class "flex-grow-1 ps-4 text-lighter fst-italic" ] [ text ("Updated " ++ updatedText) ]
                ]
            ]
        ]


viewRecentProjectsEmpty : AppState -> List (Html msg)
viewRecentProjectsEmpty appState =
    [ div [ class "text-lighter d-flex flex-column justify-content-center h-100" ]
        [ p [ class "fs-5 m-0 mt-3 text-center" ]
            [ lx_ "empty" appState
            , br [] []
            , faSet "_global.arrowRight" appState
            ]
        ]
    ]
