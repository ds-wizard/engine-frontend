module Wizard.Users.Routes exposing (Route(..), indexRouteRoleFilterId)

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)


type Route
    = CreateRoute
    | EditRoute String
    | IndexRoute PaginationQueryString (Maybe String)


indexRouteRoleFilterId : String
indexRouteRoleFilterId =
    "role"
