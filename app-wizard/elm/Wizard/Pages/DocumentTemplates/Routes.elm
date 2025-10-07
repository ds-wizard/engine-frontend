module Wizard.Pages.DocumentTemplates.Routes exposing (Route(..))

import Common.Data.PaginationQueryString exposing (PaginationQueryString)


type Route
    = DetailRoute String
    | ImportRoute (Maybe String)
    | IndexRoute PaginationQueryString
