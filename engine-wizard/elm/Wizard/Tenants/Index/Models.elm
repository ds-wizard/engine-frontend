module Wizard.Tenants.Index.Models exposing
    ( Model
    , initialModel
    )

import Shared.Data.PaginationQueryFilters as PaginationQueryFilters
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.Tenant exposing (Tenant)
import Wizard.Common.Components.Listing.Models as Listing
import Wizard.Tenants.Routes exposing (indexRouteEnabledFilterId)


type alias Model =
    { tenants : Listing.Model Tenant
    }


initialModel : PaginationQueryString -> Maybe String -> Model
initialModel paginationQueryString mbEnabled =
    let
        paginationQueryFilters =
            PaginationQueryFilters.fromValues [ ( indexRouteEnabledFilterId, mbEnabled ) ]
    in
    { tenants = Listing.initialModelWithFilters paginationQueryString paginationQueryFilters
    }
