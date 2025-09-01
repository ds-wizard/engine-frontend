module Wizard.ProjectFiles.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Wizard.ProjectFiles.Index.Models
import Wizard.ProjectFiles.Routes exposing (Route(..))


type alias Model =
    { indexModel : Wizard.ProjectFiles.Index.Models.Model
    }


initialModel : Model
initialModel =
    { indexModel = Wizard.ProjectFiles.Index.Models.initialModel PaginationQueryString.empty
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        IndexRoute paginationQuerystring ->
            { model | indexModel = Wizard.ProjectFiles.Index.Models.initialModel paginationQuerystring }
