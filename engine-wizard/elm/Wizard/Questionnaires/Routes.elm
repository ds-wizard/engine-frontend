module Wizard.Questionnaires.Routes exposing (Route(..))

import Wizard.Common.Pagination.PaginationQueryString exposing (PaginationQueryString)


type Route
    = CreateRoute (Maybe String)
    | CreateMigrationRoute String
    | DetailRoute String
    | EditRoute String
    | IndexRoute PaginationQueryString
    | MigrationRoute String
