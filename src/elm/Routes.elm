module Routes exposing (Route(..))

import KMEditor.Routes
import KnowledgeModels.Routes
import Public.Routes
import Questionnaires.Routes
import Users.Routes


type Route
    = DashboardRoute
    | KMEditorRoute KMEditor.Routes.Route
    | KnowledgeModelsRoute KnowledgeModels.Routes.Route
    | OrganizationRoute
    | PublicRoute Public.Routes.Route
    | QuestionnairesRoute Questionnaires.Routes.Route
    | UsersRoute Users.Routes.Route
    | NotAllowedRoute
    | NotFoundRoute
