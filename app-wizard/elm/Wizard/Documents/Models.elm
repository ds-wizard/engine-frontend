module Wizard.Documents.Models exposing (Model, initLocalModel, initialModel)

import Shared.Data.PaginationQueryString as PaginationQueryString
import Wizard.Documents.Index.Models
import Wizard.Documents.Routes exposing (Route(..))


type alias Model =
    { indexModel : Wizard.Documents.Index.Models.Model
    }


initialModel : Model
initialModel =
    { indexModel = Wizard.Documents.Index.Models.initialModel Nothing PaginationQueryString.empty
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        IndexRoute questionnaireUuid paginationQueryString ->
            { model | indexModel = Wizard.Documents.Index.Models.initialModel questionnaireUuid paginationQueryString }
