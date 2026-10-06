module Wizard.Pages.DocumentTemplates.Routes exposing (Route(..))

import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Pages.DocumentTemplates.Detail.DocumentTemplateDetailRoute exposing (DocumentTemplateDetailRoute)


type Route
    = DetailRoute Uuid DocumentTemplateDetailRoute
    | ImportRoute (Maybe String)
    | IndexRoute PaginationQueryString
