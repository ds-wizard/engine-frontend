module Wizard.Pages.ProjectActions.Routes exposing (Route(..))

import Common.Data.PaginationQueryString exposing (PaginationQueryString)


type Route
    = IndexRoute PaginationQueryString
