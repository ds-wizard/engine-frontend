module Wizard.Locales.Routes exposing (Route(..))

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)


type Route
    = CreateRoute
    | DetailRoute String
    | ImportRoute (Maybe String)
    | IndexRoute PaginationQueryString
