module Wizard.Documents.Models exposing (Model, initLocalModel, initialModel)

import Wizard.Common.Pagination.PaginationQueryString as PaginationQueryString
import Wizard.Documents.Create.Models
import Wizard.Documents.Index.Models
import Wizard.Documents.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.Documents.Create.Models.Model
    , indexModel : Wizard.Documents.Index.Models.Model
    }


initialModel : Model
initialModel =
    { createModel = Wizard.Documents.Create.Models.initialModel Nothing
    , indexModel = Wizard.Documents.Index.Models.initialModel Nothing PaginationQueryString.empty
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        CreateRoute selected ->
            { model | createModel = Wizard.Documents.Create.Models.initialModel selected }

        IndexRoute questionnaireUuid paginationQueryString ->
            { model | indexModel = Wizard.Documents.Index.Models.initialModel questionnaireUuid paginationQueryString }
