module Wizard.Pages.DocumentTemplates.Routes exposing (Route(..))

import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)


type Route
    = DetailRoute Uuid
    | ImportRoute (Maybe String)
    | IndexRoute PaginationQueryString
