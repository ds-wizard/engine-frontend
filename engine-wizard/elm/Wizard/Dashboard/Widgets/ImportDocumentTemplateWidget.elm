module Wizard.Dashboard.Widgets.ImportDocumentTemplateWidget exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html)
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


view : AppState -> Html msg
view appState =
    WidgetHelpers.ctaWidget appState
        { title = gettext "Import Document Template" appState.locale
        , text =
            String.format
                (gettext "Document templates transform answers from a questionnaire into a document. This document can be anything, from PDF to machine-actionable JSON. You can import existing document templates from [%s](%s/document-templates) or [develop new ones](https://guide.ds-wizard.org/en/latest/more/development/document-templates/index.html)." appState.locale)
                [ LookAndFeelConfig.defaultRegistryName, LookAndFeelConfig.defaultRegistryUrl ]
        , action =
            { route = Routes.documentTemplatesImport Nothing
            , label = gettext "Import" appState.locale
            }
        }
