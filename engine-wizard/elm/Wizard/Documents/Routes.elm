module Wizard.Documents.Routes exposing (Route(..))

import Wizard.Common.Pagination.PaginationQueryString exposing (PaginationQueryString)


type Route
    = CreateRoute (Maybe String)
    | IndexRoute (Maybe String) PaginationQueryString
