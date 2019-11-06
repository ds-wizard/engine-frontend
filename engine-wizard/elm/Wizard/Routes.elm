module Wizard.Routes exposing (Route(..))

import Wizard.KMEditor.Routes
import Wizard.KnowledgeModels.Routes
import Wizard.Public.Routes
import Wizard.Questionnaires.Routes
import Wizard.Users.Routes


type Route
    = DashboardRoute
    | KMEditorRoute Wizard.KMEditor.Routes.Route
    | KnowledgeModelsRoute Wizard.KnowledgeModels.Routes.Route
    | OrganizationRoute
    | PublicRoute Wizard.Public.Routes.Route
    | QuestionnairesRoute Wizard.Questionnaires.Routes.Route
    | UsersRoute Wizard.Users.Routes.Route
    | NotAllowedRoute
    | NotFoundRoute
