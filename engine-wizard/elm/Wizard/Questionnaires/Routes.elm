module Wizard.Questionnaires.Routes exposing (Route(..))

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)


type Route
    = CreateRoute (Maybe String)
    | CreateMigrationRoute Uuid
    | DetailRoute Uuid
    | EditRoute Uuid
    | IndexRoute PaginationQueryString
    | MigrationRoute Uuid
