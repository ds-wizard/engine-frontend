module Wizard.Projects.Detail.ProjectDetailRoute exposing (ProjectDetailRoute(..))

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)


type ProjectDetailRoute
    = Questionnaire
    | Preview
    | Metrics
    | Documents PaginationQueryString
    | NewDocument (Maybe Uuid)
    | Settings
