module Wizard.Pages.ProjectImporters.Routes exposing (Route(..))

import Common.Data.PaginationQueryString exposing (PaginationQueryString)


type Route
    = IndexRoute PaginationQueryString
