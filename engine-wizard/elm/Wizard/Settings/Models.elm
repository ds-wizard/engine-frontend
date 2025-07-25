module Wizard.Settings.Models exposing (Model, initLocalModel, initialModel)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Settings.Authentication.Models
import Wizard.Settings.DashboardAndLoginScreen.Models
import Wizard.Settings.Features.Models
import Wizard.Settings.KnowledgeModels.Models
import Wizard.Settings.LookAndFeel.Models
import Wizard.Settings.Organization.Models
import Wizard.Settings.PrivacyAndSupport.Models
import Wizard.Settings.Projects.Models
import Wizard.Settings.Registry.Models
import Wizard.Settings.Routes exposing (Route(..))
import Wizard.Settings.Submission.Models
import Wizard.Settings.Usage.Models


type alias Model =
    { organizationModel : Wizard.Settings.Organization.Models.Model
    , authenticationModel : Wizard.Settings.Authentication.Models.Model
    , privacyAndSupportModel : Wizard.Settings.PrivacyAndSupport.Models.Model
    , featuresModel : Wizard.Settings.Features.Models.Model
    , dashboardModel : Wizard.Settings.DashboardAndLoginScreen.Models.Model
    , lookAndFeelModel : Wizard.Settings.LookAndFeel.Models.Model
    , registryModel : Wizard.Settings.Registry.Models.Model
    , questionnairesModel : Wizard.Settings.Projects.Models.Model
    , documentSubmissionModel : Wizard.Settings.Submission.Models.Model
    , knowledgeModelsModel : Wizard.Settings.KnowledgeModels.Models.Model
    , usageModel : Wizard.Settings.Usage.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { organizationModel = Wizard.Settings.Organization.Models.initialModel appState
    , authenticationModel = Wizard.Settings.Authentication.Models.initialModel appState
    , privacyAndSupportModel = Wizard.Settings.PrivacyAndSupport.Models.initialModel
    , featuresModel = Wizard.Settings.Features.Models.initialModel
    , dashboardModel = Wizard.Settings.DashboardAndLoginScreen.Models.initialModel
    , lookAndFeelModel = Wizard.Settings.LookAndFeel.Models.initialModel
    , registryModel = Wizard.Settings.Registry.Models.initialModel appState
    , questionnairesModel = Wizard.Settings.Projects.Models.initialModel appState
    , documentSubmissionModel = Wizard.Settings.Submission.Models.initialModel
    , knowledgeModelsModel = Wizard.Settings.KnowledgeModels.Models.initialModel
    , usageModel = Wizard.Settings.Usage.Models.initialModel
    }


initLocalModel : AppState -> Route -> Model -> Model
initLocalModel appState route model =
    case route of
        OrganizationRoute ->
            { model | organizationModel = Wizard.Settings.Organization.Models.initialModel appState }

        AuthenticationRoute ->
            { model | authenticationModel = Wizard.Settings.Authentication.Models.initialModel appState }

        PrivacyAndSupportRoute ->
            { model | privacyAndSupportModel = Wizard.Settings.PrivacyAndSupport.Models.initialModel }

        FeaturesRoute ->
            { model | featuresModel = Wizard.Settings.Features.Models.initialModel }

        DashboardAndLoginScreenRoute ->
            { model | privacyAndSupportModel = Wizard.Settings.PrivacyAndSupport.Models.initialModel }

        LookAndFeelRoute ->
            { model | lookAndFeelModel = Wizard.Settings.LookAndFeel.Models.initialModel }

        RegistryRoute ->
            { model | registryModel = Wizard.Settings.Registry.Models.initialModel appState }

        ProjectsRoute ->
            { model | questionnairesModel = Wizard.Settings.Projects.Models.initialModel appState }

        SubmissionRoute ->
            { model | documentSubmissionModel = Wizard.Settings.Submission.Models.initialModel }

        KnowledgeModelsRoute ->
            { model | knowledgeModelsModel = Wizard.Settings.KnowledgeModels.Models.initialModel }

        UsageRoute ->
            { model | usageModel = Wizard.Settings.Usage.Models.initialModel }
