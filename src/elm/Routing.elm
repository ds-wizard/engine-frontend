module Routing exposing (..)

import Models exposing (Route(..))
import Navigation exposing (Location)
import UrlParser exposing (..)


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map IndexRoute top
        , map OrganizationRoute (s "organization")
        , map UserManagementRoute (s "user-management")
        , map KnowledgeModelsCreateRoute (s "knowledge-models" </> s "create")
        , map KnowledgeModelsEditorRoute (s "knowledge-models" </> s "edit")
        , map KnowledgeModelsRoute (s "knowledge-models")
        , map WizzardsRoute (s "wizzards")
        , map DataManagementPlansRoute (s "data-management-plans")
        , map LoginRoute (s "login")
        ]


parseLocation : Location -> Route
parseLocation location =
    case UrlParser.parsePath matchers location of
        Just route ->
            route

        Nothing ->
            NotFoundRouteRoute


indexPath : String
indexPath =
    "/"


loginPath : String
loginPath =
    "/login"


organizationPath : String
organizationPath =
    "/organization"


userManagementPath : String
userManagementPath =
    "/user-management"


knowledgeModelsPath : String
knowledgeModelsPath =
    "/knowledge-models"


knowledgeModelsCreatePath : String
knowledgeModelsCreatePath =
    "/knowledge-models/create"


wizzardsPath : String
wizzardsPath =
    "/wizzards"


dataManagementPlansPath : String
dataManagementPlansPath =
    "/data-management-plans"
