module Wizard.Pages.ProjectImporters.Routes exposing (Route(..))

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)


type Route
    = IndexRoute PaginationQueryString
