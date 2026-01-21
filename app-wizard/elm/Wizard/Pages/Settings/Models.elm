module Wizard.Pages.Settings.Models exposing (Model, initLocalModel, initialModel)

import Uuid
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Authentication.Models
import Wizard.Pages.Settings.DashboardAndLoginScreen.Models
import Wizard.Pages.Settings.Features.Models
import Wizard.Pages.Settings.KnowledgeModels.Models
import Wizard.Pages.Settings.LookAndFeel.Models
import Wizard.Pages.Settings.Organization.Models
import Wizard.Pages.Settings.PluginSettings.Model
import Wizard.Pages.Settings.Plugins.Models
import Wizard.Pages.Settings.PrivacyAndSupport.Models
import Wizard.Pages.Settings.Projects.Models
import Wizard.Pages.Settings.Registry.Models
import Wizard.Pages.Settings.Routes exposing (Route(..))
import Wizard.Pages.Settings.Submission.Models
import Wizard.Pages.Settings.Usage.Models


type alias Model =
    { organizationModel : Wizard.Pages.Settings.Organization.Models.Model
    , authenticationModel : Wizard.Pages.Settings.Authentication.Models.Model
    , privacyAndSupportModel : Wizard.Pages.Settings.PrivacyAndSupport.Models.Model
    , featuresModel : Wizard.Pages.Settings.Features.Models.Model
    , pluginsModel : Wizard.Pages.Settings.Plugins.Models.Model
    , pluginSettingsModel : Wizard.Pages.Settings.PluginSettings.Model.Model
    , dashboardModel : Wizard.Pages.Settings.DashboardAndLoginScreen.Models.Model
    , lookAndFeelModel : Wizard.Pages.Settings.LookAndFeel.Models.Model
    , registryModel : Wizard.Pages.Settings.Registry.Models.Model
    , questionnairesModel : Wizard.Pages.Settings.Projects.Models.Model
    , documentSubmissionModel : Wizard.Pages.Settings.Submission.Models.Model
    , knowledgeModelsModel : Wizard.Pages.Settings.KnowledgeModels.Models.Model
    , usageModel : Wizard.Pages.Settings.Usage.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { organizationModel = Wizard.Pages.Settings.Organization.Models.initialModel appState
    , authenticationModel = Wizard.Pages.Settings.Authentication.Models.initialModel appState
    , privacyAndSupportModel = Wizard.Pages.Settings.PrivacyAndSupport.Models.initialModel
    , featuresModel = Wizard.Pages.Settings.Features.Models.initialModel
    , pluginsModel = Wizard.Pages.Settings.Plugins.Models.initialModel appState
    , pluginSettingsModel = Wizard.Pages.Settings.PluginSettings.Model.initialModel Uuid.nil
    , dashboardModel = Wizard.Pages.Settings.DashboardAndLoginScreen.Models.initialModel
    , lookAndFeelModel = Wizard.Pages.Settings.LookAndFeel.Models.initialModel
    , registryModel = Wizard.Pages.Settings.Registry.Models.initialModel appState
    , questionnairesModel = Wizard.Pages.Settings.Projects.Models.initialModel appState
    , documentSubmissionModel = Wizard.Pages.Settings.Submission.Models.initialModel
    , knowledgeModelsModel = Wizard.Pages.Settings.KnowledgeModels.Models.initialModel
    , usageModel = Wizard.Pages.Settings.Usage.Models.initialModel
    }


initLocalModel : AppState -> Route -> Model -> Model
initLocalModel appState route model =
    case route of
        OrganizationRoute ->
            { model | organizationModel = Wizard.Pages.Settings.Organization.Models.initialModel appState }

        AuthenticationRoute ->
            { model | authenticationModel = Wizard.Pages.Settings.Authentication.Models.initialModel appState }

        PrivacyAndSupportRoute ->
            { model | privacyAndSupportModel = Wizard.Pages.Settings.PrivacyAndSupport.Models.initialModel }

        FeaturesRoute ->
            { model | featuresModel = Wizard.Pages.Settings.Features.Models.initialModel }

        PluginsRoute ->
            { model | pluginsModel = Wizard.Pages.Settings.Plugins.Models.initialModel appState }

        PluginSettingsRoute pluginUuid ->
            { model | pluginSettingsModel = Wizard.Pages.Settings.PluginSettings.Model.initialModel pluginUuid }

        DashboardAndLoginScreenRoute ->
            { model | privacyAndSupportModel = Wizard.Pages.Settings.PrivacyAndSupport.Models.initialModel }

        LookAndFeelRoute ->
            { model | lookAndFeelModel = Wizard.Pages.Settings.LookAndFeel.Models.initialModel }

        RegistryRoute ->
            { model | registryModel = Wizard.Pages.Settings.Registry.Models.initialModel appState }

        ProjectsRoute ->
            { model | questionnairesModel = Wizard.Pages.Settings.Projects.Models.initialModel appState }

        SubmissionRoute ->
            { model | documentSubmissionModel = Wizard.Pages.Settings.Submission.Models.initialModel }

        KnowledgeModelsRoute ->
            { model | knowledgeModelsModel = Wizard.Pages.Settings.KnowledgeModels.Models.initialModel }

        UsageRoute ->
            { model | usageModel = Wizard.Pages.Settings.Usage.Models.initialModel }
