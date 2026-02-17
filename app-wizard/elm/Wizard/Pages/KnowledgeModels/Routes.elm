module Wizard.Pages.KnowledgeModels.Routes exposing (Route(..))

import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)


type Route
    = DetailRoute Uuid
    | ImportRoute (Maybe String)
    | IndexRoute PaginationQueryString
    | PreviewRoute Uuid (Maybe String)
    | ResourcePageRoute Uuid String
