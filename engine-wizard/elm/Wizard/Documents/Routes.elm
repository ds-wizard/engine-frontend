module Wizard.Documents.Routes exposing (Route(..))

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)


type Route
    = CreateRoute (Maybe Uuid)
    | IndexRoute (Maybe Uuid) PaginationQueryString
