module Wizard.Users.Routes exposing (Route(..))

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)


type Route
    = CreateRoute
    | EditRoute String
    | IndexRoute PaginationQueryString
