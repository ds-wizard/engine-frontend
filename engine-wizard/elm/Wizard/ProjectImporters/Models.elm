module Wizard.ProjectImporters.Models exposing (Model, initLocalModel, initialModel)

import Shared.Data.PaginationQueryString as PaginationQueryString
import Wizard.ProjectImporters.Index.Models
import Wizard.ProjectImporters.Routes exposing (Route(..))


type alias Model =
    { indexModel : Wizard.ProjectImporters.Index.Models.Model
    }


initialModel : Model
initialModel =
    { indexModel = Wizard.ProjectImporters.Index.Models.initialModel PaginationQueryString.empty
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        IndexRoute paginationQuerystring ->
            { model | indexModel = Wizard.ProjectImporters.Index.Models.initialModel paginationQuerystring }
