module Wizard.Dashboard.Widgets.CreateProjectWidget exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


view : AppState -> Html msg
view appState =
    WidgetHelpers.ctaWidget
        { title = gettext "Create Project" appState.locale
        , text = gettext "Project is a workspace where you create your DMP. It is based on a knowledge model, which contains knowledge about what should be asked and how based on the research field or organization's needs. You can use document templates to transform the answers into a document. This document can be anything, from PDF to machine-actionable JSON.\n\nYou can create a new project from a project template that data stewards prepare for you to have an easier start or from scratch where you set up everything yourself." appState.locale
        , action =
            { route = Routes.projectsCreate
            , label = gettext "Create" appState.locale
            }
        }
