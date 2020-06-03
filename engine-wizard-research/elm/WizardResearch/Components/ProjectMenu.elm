module WizardResearch.Components.ProjectMenu exposing (..)

import Html.Styled exposing (Html)
import Html.Styled.Attributes exposing (href)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Elemental.Components.SideNavigation as SideNavigation
import WizardResearch.Common.AppState exposing (AppState)
import WizardResearch.Route as Route
import WizardResearch.Route.ProjectRoute as ProjectRoute


type ProjectMenuActivePage
    = Overview
    | Planning
    | Starred
    | Metrics
    | Documents
    | Settings


view : AppState -> Questionnaire -> ProjectMenuActivePage -> Html msg
view appState questionnaire activePage =
    SideNavigation.view []
        [ SideNavigation.projectName appState.theme questionnaire.name
        , SideNavigation.itemDefault appState.theme
            { icon = "far fa-folder-open"
            , label = "Overview"
            , badge = Nothing
            , selected = activePage == Overview
            }
            [ href (Route.toString (Route.Project questionnaire.uuid ProjectRoute.Overview)) ]
        , SideNavigation.item appState.theme
            { icon = "far fa-comments"
            , label = "Planning"
            , badge = Nothing
            , selected = activePage == Planning
            , caret = SideNavigation.CaretClosed
            }
            [ href (Route.toString (Route.Project questionnaire.uuid ProjectRoute.Planning)) ]
        , SideNavigation.itemDefault appState.theme
            { icon = "far fa-star"
            , label = "Starred"
            , badge = Nothing
            , selected = activePage == Starred
            }
            [ href (Route.toString (Route.Project questionnaire.uuid ProjectRoute.Starred)) ]
        , SideNavigation.itemDefault appState.theme
            { icon = "far fa-chart-bar"
            , label = "Metrics"
            , badge = Nothing
            , selected = activePage == Metrics
            }
            [ href (Route.toString (Route.Project questionnaire.uuid ProjectRoute.Metrics)) ]
        , SideNavigation.itemDefault appState.theme
            { icon = "far fa-copy"
            , label = "Documents"
            , badge = Nothing
            , selected = activePage == Documents
            }
            [ href (Route.toString (Route.Project questionnaire.uuid ProjectRoute.Documents)) ]
        , SideNavigation.item appState.theme
            { icon = "fas fa-cogs"
            , label = "Settings"
            , badge = Nothing
            , selected = activePage == Settings
            , caret = SideNavigation.CaretClosed
            }
            [ href (Route.toString (Route.Project questionnaire.uuid ProjectRoute.Settings)) ]
        ]
