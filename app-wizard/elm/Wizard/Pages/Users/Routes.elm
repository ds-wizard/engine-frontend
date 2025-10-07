module Wizard.Pages.Users.Routes exposing (Route(..), indexRouteRoleFilterId)

import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Common.Data.UuidOrCurrent exposing (UuidOrCurrent)
import Wizard.Pages.Users.Edit.UserEditRoutes exposing (UserEditRoute)


type Route
    = CreateRoute
    | EditRoute UuidOrCurrent UserEditRoute
    | IndexRoute PaginationQueryString (Maybe String)


indexRouteRoleFilterId : String
indexRouteRoleFilterId =
    "role"
