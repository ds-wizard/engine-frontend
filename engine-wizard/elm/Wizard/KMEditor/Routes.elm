module Wizard.KMEditor.Routes exposing (Route(..))

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)


type Route
    = CreateRoute (Maybe String)
    | EditorRoute Uuid
    | IndexRoute PaginationQueryString
    | MigrationRoute Uuid
    | PublishRoute Uuid
