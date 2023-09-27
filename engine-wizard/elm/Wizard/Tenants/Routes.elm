module Wizard.Tenants.Routes exposing
    ( Route(..)
    , indexRouteEnabledFilterId
    )

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)


type Route
    = IndexRoute PaginationQueryString (Maybe String)
    | CreateRoute
    | DetailRoute Uuid


indexRouteEnabledFilterId : String
indexRouteEnabledFilterId =
    "enabled"
