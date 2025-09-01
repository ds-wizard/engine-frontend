module Wizard.Tenants.Index.Models exposing
    ( Model
    , initialModel
    )

import Shared.Data.PaginationQueryFilters as PaginationQueryFilters
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.Tenant exposing (Tenant)
import Wizard.Common.Components.Listing.Models as Listing
import Wizard.Tenants.Routes exposing (indexRouteEnabledFilterId, indexRouteStatesFilterId)


type alias Model =
    { tenants : Listing.Model Tenant
    }


initialModel : PaginationQueryString -> Maybe String -> Maybe String -> Model
initialModel paginationQueryString mbEnabled mbStates =
    let
        paginationQueryFilters =
            PaginationQueryFilters.fromValues
                [ ( indexRouteEnabledFilterId, mbEnabled )
                , ( indexRouteStatesFilterId, mbStates )
                ]
    in
    { tenants = Listing.initialModelWithFilters paginationQueryString paginationQueryFilters
    }
