module Wizard.DocumentTemplates.Routes exposing (Route(..))

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)


type Route
    = DetailRoute String
    | ImportRoute (Maybe String)
    | IndexRoute PaginationQueryString
