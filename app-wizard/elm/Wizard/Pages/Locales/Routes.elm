module Wizard.Pages.Locales.Routes exposing (Route(..))

import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)


type Route
    = CreateRoute
    | DetailRoute Uuid
    | ImportRoute (Maybe String)
    | IndexRoute PaginationQueryString
