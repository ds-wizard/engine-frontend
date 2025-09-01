module Wizard.Pages.ProjectImporters.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Wizard.Pages.ProjectImporters.Index.Models
import Wizard.Pages.ProjectImporters.Routes exposing (Route(..))


type alias Model =
    { indexModel : Wizard.Pages.ProjectImporters.Index.Models.Model
    }


initialModel : Model
initialModel =
    { indexModel = Wizard.Pages.ProjectImporters.Index.Models.initialModel PaginationQueryString.empty
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        IndexRoute paginationQuerystring ->
            { model | indexModel = Wizard.Pages.ProjectImporters.Index.Models.initialModel paginationQuerystring }
