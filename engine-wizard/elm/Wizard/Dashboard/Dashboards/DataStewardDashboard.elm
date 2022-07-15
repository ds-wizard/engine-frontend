module Wizard.Dashboard.Dashboards.DataStewardDashboard exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Widgets.CreateKnowledgeModelWidget as CreateKnowledgeModelWidget
import Wizard.Dashboard.Widgets.CreateProjectTemplateWidget as CreateProjectTemplateWidget
import Wizard.Dashboard.Widgets.ImportDocumentTemplateWidget as ImportDocumentTemplateWidget
import Wizard.Dashboard.Widgets.ImportKnowledgeModelWidget as ImportKnowledgeModelWidget
import Wizard.Dashboard.Widgets.WelcomeWidget as WelcomeWidget


view : AppState -> Html msg
view appState =
    div []
        [ div [ class "row gx-3" ]
            [ WelcomeWidget.view appState
            , CreateKnowledgeModelWidget.view appState
            , CreateProjectTemplateWidget.view appState
            , ImportKnowledgeModelWidget.view appState
            , ImportDocumentTemplateWidget.view appState
            ]
        ]
