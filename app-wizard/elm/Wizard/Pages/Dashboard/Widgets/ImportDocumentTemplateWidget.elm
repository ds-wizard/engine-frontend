module Wizard.Pages.Dashboard.Widgets.ImportDocumentTemplateWidget exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html)
import String.Format as String
import Wizard.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Html msg
view appState =
    WidgetHelpers.ctaWidget
        { title = gettext "Import Document Template" appState.locale
        , text =
            String.format
                (gettext "Document templates transform answers from a questionnaire into a document. This document can be anything, from PDF to machine-actionable JSON. You can import existing document templates from [%s](%s) or [develop new ones](%s)." appState.locale)
                [ LookAndFeelConfig.defaultRegistryName
                , LookAndFeelConfig.defaultRegistryUrl ++ "/document-templates"
                , WizardGuideLinks.documentTemplates appState.guideLinks
                ]
        , action =
            { route = Routes.documentTemplatesImport Nothing
            , label = gettext "Import" appState.locale
            }
        }
