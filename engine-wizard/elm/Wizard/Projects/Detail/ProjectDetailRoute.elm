module Wizard.Projects.Detail.ProjectDetailRoute exposing (ProjectDetailRoute(..))

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)


type ProjectDetailRoute
    = Questionnaire
    | Preview
    | Metrics
    | Documents PaginationQueryString
    | NewDocument (Maybe String)
    | Settings
