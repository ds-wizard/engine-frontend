module Wizard.Pages.KnowledgeModels.Routes exposing (Route(..))

import Common.Data.PaginationQueryString exposing (PaginationQueryString)


type Route
    = DetailRoute String
    | ImportRoute (Maybe String)
    | IndexRoute PaginationQueryString
    | PreviewRoute String (Maybe String)
    | ResourcePageRoute String String
