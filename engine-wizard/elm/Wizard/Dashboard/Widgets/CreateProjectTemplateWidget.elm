module Wizard.Dashboard.Widgets.CreateProjectTemplateWidget exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


view : AppState -> Html msg
view appState =
    WidgetHelpers.ctaWidget appState
        { title = gettext "Create Project Template" appState.locale
        , text = gettext "Project template is a special type of project that can be used as a starting point for new projects. You can select a knowledge model, set up a document template and format, or prefill some answers to make it easier to start for researchers." appState.locale
        , action =
            { route = Routes.projectsCreate
            , label = gettext "Create" appState.locale
            }
        }
