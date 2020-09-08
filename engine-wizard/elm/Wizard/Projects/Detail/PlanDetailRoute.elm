module Wizard.Projects.Detail.PlanDetailRoute exposing (PlanDetailRoute(..))

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)


type PlanDetailRoute
    = Questionnaire
    | Preview
    | TODOs
    | Metrics
    | Documents PaginationQueryString
    | NewDocument
    | Settings
