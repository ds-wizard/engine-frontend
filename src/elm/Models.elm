module Models exposing (..)


type Route
    = IndexRoute
    | OrganizationRoute
    | UserManagementRoute
    | KnowledgeModelsRoute
    | WizzardsRoute
    | DataManagementPlansRoute
    | NotFoundRouteRoute


type alias Model =
    { route : Route
    }


initialModel : Route -> Model
initialModel route =
    { route = route
    }
