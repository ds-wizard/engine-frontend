module Wizard.Routes exposing (Route(..))

import Wizard.Documents.Routes
import Wizard.KMEditor.Routes
import Wizard.KnowledgeModels.Routes
import Wizard.Public.Routes
import Wizard.Questionnaires.Routes
import Wizard.Registry.Routes
import Wizard.Settings.Routes
import Wizard.Templates.Routes
import Wizard.Users.Routes


type Route
    = DashboardRoute
    | DocumentsRoute Wizard.Documents.Routes.Route
    | KMEditorRoute Wizard.KMEditor.Routes.Route
    | KnowledgeModelsRoute Wizard.KnowledgeModels.Routes.Route
    | PublicRoute Wizard.Public.Routes.Route
    | QuestionnairesRoute Wizard.Questionnaires.Routes.Route
    | RegistryRoute Wizard.Registry.Routes.Route
    | SettingsRoute Wizard.Settings.Routes.Route
    | TemplatesRoute Wizard.Templates.Routes.Route
    | UsersRoute Wizard.Users.Routes.Route
    | NotAllowedRoute
    | NotFoundRoute
