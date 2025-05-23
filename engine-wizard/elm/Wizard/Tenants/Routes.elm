module Wizard.Tenants.Routes exposing
    ( Route(..)
    , indexRouteEnabledFilterId
    , indexRouteStatesFilterId
    )

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)


type Route
    = IndexRoute PaginationQueryString (Maybe String) (Maybe String)
    | CreateRoute
    | DetailRoute Uuid


indexRouteEnabledFilterId : String
indexRouteEnabledFilterId =
    "enabled"


indexRouteStatesFilterId : String
indexRouteStatesFilterId =
    "states"
