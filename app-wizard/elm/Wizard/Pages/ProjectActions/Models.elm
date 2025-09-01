module Wizard.Pages.ProjectActions.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Wizard.Pages.ProjectActions.Index.Models
import Wizard.Pages.ProjectActions.Routes exposing (Route(..))


type alias Model =
    { indexModel : Wizard.Pages.ProjectActions.Index.Models.Model
    }


initialModel : Model
initialModel =
    { indexModel = Wizard.Pages.ProjectActions.Index.Models.initialModel PaginationQueryString.empty
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        IndexRoute paginationQuerystring ->
            { model | indexModel = Wizard.Pages.ProjectActions.Index.Models.initialModel paginationQuerystring }
