module Wizard.DocumentTemplateEditors.Routes exposing (Route(..))

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)


type Route
    = CreateRoute (Maybe String) (Maybe Bool)
    | IndexRoute PaginationQueryString
    | EditorRoute String
