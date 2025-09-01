module Wizard.Pages.KMEditor.Routes exposing (Route(..))

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Pages.KMEditor.Editor.KMEditorRoute exposing (KMEditorRoute)


type Route
    = CreateRoute (Maybe String) (Maybe Bool)
    | EditorRoute Uuid KMEditorRoute
    | IndexRoute PaginationQueryString
    | MigrationRoute Uuid
    | PublishRoute Uuid
