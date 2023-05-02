module Wizard.Users.Routes exposing (Route(..), indexRouteRoleFilterId)

import Shared.Common.UuidOrCurrent exposing (UuidOrCurrent)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Users.Edit.UserEditRoutes exposing (UserEditRoute)


type Route
    = CreateRoute
    | EditRoute UuidOrCurrent UserEditRoute
    | IndexRoute PaginationQueryString (Maybe String)


indexRouteRoleFilterId : String
indexRouteRoleFilterId =
    "role"
