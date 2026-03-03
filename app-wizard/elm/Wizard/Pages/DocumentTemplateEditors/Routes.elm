module Wizard.Pages.DocumentTemplateEditors.Routes exposing (Route(..))

import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Pages.DocumentTemplateEditors.Editor.DTEditorRoute exposing (DTEditorRoute)


type Route
    = CreateRoute (Maybe Uuid) (Maybe Bool)
    | IndexRoute PaginationQueryString
    | EditorRoute Uuid DTEditorRoute
