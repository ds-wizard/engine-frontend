module Wizard.ProjectActions.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Wizard.ProjectActions.Index.Models
import Wizard.ProjectActions.Routes exposing (Route(..))


type alias Model =
    { indexModel : Wizard.ProjectActions.Index.Models.Model
    }


initialModel : Model
initialModel =
    { indexModel = Wizard.ProjectActions.Index.Models.initialModel PaginationQueryString.empty
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        IndexRoute paginationQuerystring ->
            { model | indexModel = Wizard.ProjectActions.Index.Models.initialModel paginationQuerystring }
