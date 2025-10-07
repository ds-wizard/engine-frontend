module Wizard.Pages.DocumentTemplateEditors.Routes exposing (Route(..))

import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Pages.DocumentTemplateEditors.Editor.DTEditorRoute exposing (DTEditorRoute)


type Route
    = CreateRoute (Maybe String) (Maybe Bool)
    | IndexRoute PaginationQueryString
    | EditorRoute String DTEditorRoute
