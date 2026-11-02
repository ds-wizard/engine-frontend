module Common.Pages.Dashboard.Widgets.CallToAction exposing
    ( addOpenId
    , configureLookAndFeel
    , connectRegistry
    , createKnowledgeModel
    , createProject
    , createProjectTemplate
    , importDocumentTemplate
    , importKnowledgeModel
    )

{-| The widgets that suggest a next step: what the thing is, and a link to go
and do it.

Each one is `Nothing` when the wizard would not show it — the same permissions
and the same "not configured yet" checks. The link is a real `a`
(`Ui.Button.linkPrimary`), because every one of them navigates.

The text is markdown, as in the wizard: some of it links to the registry or the
guide, and one of them has two paragraphs.

-}

import Common.Api.Models.BootstrapConfig.RegistryConfig as RegistryConfig
import Common.Components.Markdown as Markdown
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.GuideLinks as GuideLinks
import Common.Data.WizardRolePermission as RolePermission
import Common.Routes as Routes
import Gettext exposing (gettext)
import Html exposing (div)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import String.Format as String
import Ui.Button as Button
import Ui.Layout as Layout
import Ui.Widget as Widget exposing (Widget)


type alias Config =
    { title : String
    , text : String
    , label : String
    , route : Routes.Route
    }


widgetIf : AppState -> Bool -> Config -> Maybe (Widget msg)
widgetIf appState isEnabled config =
    if isEnabled && Routes.isReachable appState config.route then
        Just (widget appState config)

    else
        Nothing


widget : AppState -> Config -> Widget msg
widget appState config =
    Widget.widget
        { title = config.title
        , body =
            -- The widget appState's title is an h2, so headings in the text start below it.
            [ div [ class "c-dashboard-cta__text" ] [ Markdown.view { headingOffset = 2 } config.text ]
            , Layout.inline [ Button.toHtml (Button.linkPrimary config.label (Routes.toUrl appState config.route)) ]
            ]
        }


createProject : AppState -> Maybe (Widget msg)
createProject appState =
    widgetIf appState
        (Feature.projectsCreate appState)
        { title = gettext "Create Project" appState.locale
        , text = gettext "Project is a workspace where you create your DMP. It is based on a knowledge model, which contains knowledge about what should be asked and how based on the research field or organization's needs. You can use document templates to transform the answers into a document. This document can be anything, from PDF to machine-actionable JSON.\n\nYou can create a new project from a project template that data stewards prepare for you to have an easier start or from scratch where you set up everything yourself." appState.locale
        , label = gettext "Create" appState.locale
        , route = Routes.projectsCreate
        }


createProjectTemplate : AppState -> Maybe (Widget msg)
createProjectTemplate appState =
    widgetIf appState
        (AppState.userHasPerm RolePermission.projectTemplatesManage appState && Feature.projectsCreate appState)
        { title = gettext "Create Project Template" appState.locale
        , text = gettext "Project template is a special type of project that can be used as a starting point for new projects. You can select a knowledge model, set up a document template and format, or prefill some answers to make it easier to start for researchers." appState.locale
        , label = gettext "Create" appState.locale
        , route = Routes.projectsCreate
        }


createKnowledgeModel : AppState -> Maybe (Widget msg)
createKnowledgeModel appState =
    widgetIf appState
        (AppState.userHasPerm RolePermission.knowledgeModelEditorsUse appState)
        { title = gettext "Create Knowledge Model" appState.locale
        , text = gettext "Knowledge model is a tree-like structure containing the knowledge about what should be asked and how. It is used as a template for questionnaires in projects. You can create new ones from scratch or by modifying existing ones." appState.locale
        , label = gettext "Create" appState.locale
        , route = Routes.kmEditorCreate Nothing Nothing
        }


importKnowledgeModel : AppState -> Maybe (Widget msg)
importKnowledgeModel appState =
    widgetIf appState
        (AppState.userHasPerm RolePermission.knowledgeModelsManage appState)
        { title = gettext "Import Knowledge Model" appState.locale
        , text =
            String.format
                (gettext "Knowledge models are published in [%s](%s). You can easily import them into your instance to make them available for researchers. You can also import other knowledge models exported from different instances." appState.locale)
                [ appState.clientDefaults.registryName, appState.clientDefaults.registryUrl ++ "/knowledge-models" ]
        , label = gettext "Import" appState.locale
        , route = Routes.knowledgeModelsImport Nothing
        }


importDocumentTemplate : AppState -> Maybe (Widget msg)
importDocumentTemplate appState =
    widgetIf appState
        (AppState.userHasPerm RolePermission.documentTemplatesManage appState)
        { title = gettext "Import Document Template" appState.locale
        , text =
            String.format
                (gettext "Document templates transform answers from a questionnaire into a document. This document can be anything, from PDF to machine-actionable JSON. You can import existing document templates from [%s](%s) or [develop new ones](%s)." appState.locale)
                [ appState.clientDefaults.registryName
                , appState.clientDefaults.registryUrl ++ "/document-templates"
                , GuideLinks.documentTemplates appState.guideLinks
                ]
        , label = gettext "Import" appState.locale
        , route = Routes.documentTemplatesImport Nothing
        }


configureLookAndFeel : AppState -> Maybe (Widget msg)
configureLookAndFeel appState =
    widgetIf appState
        (AppState.userHasPerm RolePermission.settingsManage appState
            && Maybe.isNothing appState.config.lookAndFeel.appTitle
        )
        { title = gettext "Configure Look & Feel" appState.locale
        , text = gettext "You can configure the application name, its color and its logo." appState.locale
        , label = gettext "Configure" appState.locale
        , route = Routes.settingsLookAndFeel
        }


connectRegistry : AppState -> Maybe (Widget msg)
connectRegistry appState =
    widgetIf appState
        (AppState.userHasPerm RolePermission.settingsManage appState
            && not (RegistryConfig.isEnabled appState.config.registry)
        )
        { title = gettext "Connect to DSW Registry" appState.locale
        , text = gettext "[DSW Registry](https://registry.ds-wizard.org) is a place for published knowledge models and document templates. When you connect your instance, data stewards can easily import them." appState.locale
        , label = gettext "Connect" appState.locale
        , route = Routes.settingsRegistry
        }


addOpenId : AppState -> Maybe (Widget msg)
addOpenId appState =
    widgetIf appState
        (AppState.userHasPerm RolePermission.settingsManage appState
            && List.isEmpty appState.config.authentication.services
        )
        { title = gettext "Add OpenID Service" appState.locale
        , text = gettext "OpenID is an open standard for authentication that allows using existing accounts to login to other services without creating a new password. Configure your organization's OpenID to make it easier for your users to log in." appState.locale
        , label = gettext "Add OpenID" appState.locale
        , route = Routes.settingsAuthentication
        }
