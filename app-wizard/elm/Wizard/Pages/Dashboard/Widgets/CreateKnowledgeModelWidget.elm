module Wizard.Pages.Dashboard.Widgets.CreateKnowledgeModelWidget exposing (enabled, view)

import Common.Data.WizardRolePermission as RolePermission
import Gettext exposing (gettext)
import Html exposing (Html)
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


enabled : AppState -> Bool
enabled appState =
    AppState.userHasPerm RolePermission.knowledgeModelEditorsUse appState


view : AppState -> Html msg
view appState =
    WidgetHelpers.ctaWidget
        { title = gettext "Create Knowledge Model" appState.locale
        , text = gettext "Knowledge model is a tree-like structure containing the knowledge about what should be asked and how. It is used as a template for questionnaires in projects. You can create new ones from scratch or by modifying existing ones." appState.locale
        , action =
            { route = Routes.kmEditorCreate Nothing Nothing
            , label = gettext "Create" appState.locale
            }
        }
