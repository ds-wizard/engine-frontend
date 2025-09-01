module Wizard.Pages.ProjectFiles.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Wizard.Pages.ProjectFiles.Index.Models
import Wizard.Pages.ProjectFiles.Routes exposing (Route(..))


type alias Model =
    { indexModel : Wizard.Pages.ProjectFiles.Index.Models.Model
    }


initialModel : Model
initialModel =
    { indexModel = Wizard.Pages.ProjectFiles.Index.Models.initialModel PaginationQueryString.empty
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        IndexRoute paginationQuerystring ->
            { model | indexModel = Wizard.Pages.ProjectFiles.Index.Models.initialModel paginationQuerystring }
