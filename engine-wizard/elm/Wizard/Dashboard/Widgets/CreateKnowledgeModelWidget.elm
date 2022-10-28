module Wizard.Dashboard.Widgets.CreateKnowledgeModelWidget exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


view : AppState -> Html msg
view appState =
    WidgetHelpers.ctaWidget appState
        { title = gettext "Create Knowledge Model" appState.locale
        , text = gettext "Knowledge model is a tree-like structure containing the knowledge about what should be asked and how. It is used as a template for questionnaires in projects. You can create new ones from scratch or by modifying existing ones." appState.locale
        , action =
            { route = Routes.kmEditorCreate Nothing Nothing
            , label = gettext "Create" appState.locale
            }
        }
