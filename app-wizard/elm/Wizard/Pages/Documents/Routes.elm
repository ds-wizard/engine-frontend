module Wizard.Pages.Documents.Routes exposing (Route(..))

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)


type Route
    = IndexRoute (Maybe Uuid) PaginationQueryString
