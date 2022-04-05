module Wizard.Settings.Models exposing (Model, initLocalModel, initialModel)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Settings.Authentication.Models
import Wizard.Settings.Dashboard.Models
import Wizard.Settings.KnowledgeModels.Models
import Wizard.Settings.LookAndFeel.Models
import Wizard.Settings.Organization.Models
import Wizard.Settings.Plans.Models
import Wizard.Settings.PrivacyAndSupport.Models
import Wizard.Settings.Projects.Models
import Wizard.Settings.Registry.Models
import Wizard.Settings.Routes exposing (Route(..))
import Wizard.Settings.Submission.Models
import Wizard.Settings.Template.Models
import Wizard.Settings.Usage.Models


type alias Model =
    { organizationModel : Wizard.Settings.Organization.Models.Model
    , authenticationModel : Wizard.Settings.Authentication.Models.Model
    , privacyAndSupportModel : Wizard.Settings.PrivacyAndSupport.Models.Model
    , dashboardModel : Wizard.Settings.Dashboard.Models.Model
    , lookAndFeelModel : Wizard.Settings.LookAndFeel.Models.Model
    , registryModel : Wizard.Settings.Registry.Models.Model
    , questionnairesModel : Wizard.Settings.Projects.Models.Model
    , documentSubmissionModel : Wizard.Settings.Submission.Models.Model
    , templateModel : Wizard.Settings.Template.Models.Model
    , knowledgeModelsModel : Wizard.Settings.KnowledgeModels.Models.Model
    , usageModel : Wizard.Settings.Usage.Models.Model
    , plansModel : Wizard.Settings.Plans.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { organizationModel = Wizard.Settings.Organization.Models.initialModel
    , authenticationModel = Wizard.Settings.Authentication.Models.initialModel
    , privacyAndSupportModel = Wizard.Settings.PrivacyAndSupport.Models.initialModel
    , dashboardModel = Wizard.Settings.Dashboard.Models.initialModel
    , lookAndFeelModel = Wizard.Settings.LookAndFeel.Models.initialModel
    , registryModel = Wizard.Settings.Registry.Models.initialModel
    , questionnairesModel = Wizard.Settings.Projects.Models.initialModel appState
    , documentSubmissionModel = Wizard.Settings.Submission.Models.initialModel
    , templateModel = Wizard.Settings.Template.Models.initialModel
    , knowledgeModelsModel = Wizard.Settings.KnowledgeModels.Models.initialModel
    , usageModel = Wizard.Settings.Usage.Models.initialModel
    , plansModel = Wizard.Settings.Plans.Models.initialModel
    }


initLocalModel : AppState -> Route -> Model -> Model
initLocalModel appState route model =
    case route of
        OrganizationRoute ->
            { model | organizationModel = Wizard.Settings.Organization.Models.initialModel }

        AuthenticationRoute ->
            { model | authenticationModel = Wizard.Settings.Authentication.Models.initialModel }

        PrivacyAndSupportRoute ->
            { model | privacyAndSupportModel = Wizard.Settings.PrivacyAndSupport.Models.initialModel }

        DashboardRoute ->
            { model | privacyAndSupportModel = Wizard.Settings.PrivacyAndSupport.Models.initialModel }

        LookAndFeelRoute ->
            { model | lookAndFeelModel = Wizard.Settings.LookAndFeel.Models.initialModel }

        RegistryRoute ->
            { model | registryModel = Wizard.Settings.Registry.Models.initialModel }

        ProjectsRoute ->
            { model | questionnairesModel = Wizard.Settings.Projects.Models.initialModel appState }

        SubmissionRoute ->
            { model | documentSubmissionModel = Wizard.Settings.Submission.Models.initialModel }

        TemplateRoute ->
            { model | templateModel = Wizard.Settings.Template.Models.initialModel }

        KnowledgeModelsRoute ->
            { model | knowledgeModelsModel = Wizard.Settings.KnowledgeModels.Models.initialModel }

        UsageRoute ->
            { model | usageModel = Wizard.Settings.Usage.Models.initialModel }

        PlansRoute ->
            { model | plansModel = Wizard.Settings.Plans.Models.initialModel }
