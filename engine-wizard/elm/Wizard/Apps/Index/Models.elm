module Wizard.Apps.Index.Models exposing
    ( Model
    , initialModel
    )

import Shared.Data.App exposing (App)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Apps.Routes exposing (indexRouteEnabledFilterId)
import Wizard.Common.Components.Listing.Models as Listing


type alias Model =
    { apps : Listing.Model App
    }


initialModel : PaginationQueryString -> Maybe String -> Model
initialModel paginationQueryString mbEnabled =
    let
        paginationQueryFilters =
            PaginationQueryFilters.fromValues [ ( indexRouteEnabledFilterId, mbEnabled ) ]
    in
    { apps = Listing.initialModelWithFilters paginationQueryString paginationQueryFilters
    }
