module Common.Pages.Projects.Detail.Routes exposing (ProjectDetailRoute(..))

import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)


type ProjectDetailRoute
    = Questionnaire (Maybe String) (Maybe Uuid)
    | Preview
    | Metrics
    | Documents PaginationQueryString
    | NewDocument (Maybe Uuid)
    | Files PaginationQueryString
    | Settings
    | Plugin String
