module Wizard.Pages.ProjectFiles.Routes exposing (Route(..))

import Common.Data.PaginationQueryString exposing (PaginationQueryString)


type Route
    = IndexRoute PaginationQueryString
