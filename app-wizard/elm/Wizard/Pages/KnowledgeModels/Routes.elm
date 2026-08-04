module Wizard.Pages.KnowledgeModels.Routes exposing (Route(..))

import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Pages.KnowledgeModels.Detail.KnowledgeModelDetailRoute exposing (KnowledgeModelDetailRoute)


type Route
    = DetailRoute Uuid KnowledgeModelDetailRoute
    | ImportRoute (Maybe String)
    | IndexRoute PaginationQueryString
    | PreviewRoute Uuid (Maybe String)
    | ResourcePageRoute Uuid String
    | CompareRoute (Maybe Uuid)
