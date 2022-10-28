module Wizard.Dashboard.Widgets.ImportDocumentTemplateWidget exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


view : AppState -> Html msg
view appState =
    WidgetHelpers.ctaWidget appState
        { title = gettext "Import Document Template" appState.locale
        , text = gettext "Document templates transform answers from a questionnaire into a document. This document can be anything, from PDF to machine-actionable JSON. You can import existing document templates from [DSW Registry](https://registry.ds-wizard.org/templates) or create new ones using [DSW TDK](https://guide.ds-wizard.org/miscellaneous/development/template-development)." appState.locale
        , action =
            { route = Routes.templatesImport Nothing
            , label = gettext "Import" appState.locale
            }
        }
