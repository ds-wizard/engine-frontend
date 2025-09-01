module Wizard.Users.Routes exposing (Route(..), indexRouteRoleFilterId)

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.UuidOrCurrent exposing (UuidOrCurrent)
import Wizard.Users.Edit.UserEditRoutes exposing (UserEditRoute)


type Route
    = CreateRoute
    | EditRoute UuidOrCurrent UserEditRoute
    | IndexRoute PaginationQueryString (Maybe String)


indexRouteRoleFilterId : String
indexRouteRoleFilterId =
    "role"
