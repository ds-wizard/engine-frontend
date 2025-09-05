module Wizard.Pages.Documents.Models exposing (Model, initLocalModel, initialModel)

import Common.Data.PaginationQueryString as PaginationQueryString
import Wizard.Pages.Documents.Index.Models
import Wizard.Pages.Documents.Routes exposing (Route(..))


type alias Model =
    { indexModel : Wizard.Pages.Documents.Index.Models.Model
    }


initialModel : Model
initialModel =
    { indexModel = Wizard.Pages.Documents.Index.Models.initialModel Nothing PaginationQueryString.empty
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        IndexRoute questionnaireUuid paginationQueryString ->
            { model | indexModel = Wizard.Pages.Documents.Index.Models.initialModel questionnaireUuid paginationQueryString }
