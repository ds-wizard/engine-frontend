module Wizard.Dashboard.Widgets.ImportDocumentTemplateWidget exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html)
import String.Format as String
import Wizard.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.GuideLinks as GuideLinks
import Wizard.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


view : AppState -> Html msg
view appState =
    WidgetHelpers.ctaWidget
        { title = gettext "Import Document Template" appState.locale
        , text =
            String.format
                (gettext "Document templates transform answers from a questionnaire into a document. This document can be anything, from PDF to machine-actionable JSON. You can import existing document templates from [%s](%s) or [develop new ones](%s)." appState.locale)
                [ LookAndFeelConfig.defaultRegistryName
                , LookAndFeelConfig.defaultRegistryUrl ++ "/document-templates"
                , GuideLinks.documentTemplates appState.guideLinks
                ]
        , action =
            { route = Routes.documentTemplatesImport Nothing
            , label = gettext "Import" appState.locale
            }
        }
