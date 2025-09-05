module Wizard.Pages.Tenants.Routes exposing
    ( Route(..)
    , indexRouteEnabledFilterId
    , indexRouteStatesFilterId
    )

import Common.Data.PaginationQueryString exposing (PaginationQueryString)
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
