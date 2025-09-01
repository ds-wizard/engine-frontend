module Wizard.Pages.ProjectFiles.Routes exposing (Route(..))

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)


type Route
    = IndexRoute PaginationQueryString
