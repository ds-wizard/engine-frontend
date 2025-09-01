module Wizard.Projects.Detail.ProjectDetailRoute exposing (ProjectDetailRoute(..))

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)


type ProjectDetailRoute
    = Questionnaire (Maybe String) (Maybe Uuid)
    | Preview
    | Metrics
    | Documents PaginationQueryString
    | NewDocument (Maybe Uuid)
    | Files PaginationQueryString
    | Settings
