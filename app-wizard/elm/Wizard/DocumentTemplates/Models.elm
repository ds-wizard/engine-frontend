module Wizard.DocumentTemplates.Models exposing (Model, initLocalModel, initialModel)

import Shared.Data.PaginationQueryString as PaginationQueryString
import Wizard.Common.AppState exposing (AppState)
import Wizard.DocumentTemplates.Detail.Models
import Wizard.DocumentTemplates.Import.Models
import Wizard.DocumentTemplates.Index.Models
import Wizard.DocumentTemplates.Routes exposing (Route(..))


type alias Model =
    { detailModel : Wizard.DocumentTemplates.Detail.Models.Model
    , importModel : Wizard.DocumentTemplates.Import.Models.Model
    , indexModel : Wizard.DocumentTemplates.Index.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { detailModel = Wizard.DocumentTemplates.Detail.Models.initialModel
    , importModel = Wizard.DocumentTemplates.Import.Models.initialModel appState Nothing
    , indexModel = Wizard.DocumentTemplates.Index.Models.initialModel PaginationQueryString.empty
    }


initLocalModel : Route -> AppState -> Model -> Model
initLocalModel route appState model =
    case route of
        DetailRoute _ ->
            { model | detailModel = Wizard.DocumentTemplates.Detail.Models.initialModel }

        ImportRoute packageId ->
            { model | importModel = Wizard.DocumentTemplates.Import.Models.initialModel appState packageId }

        IndexRoute paginationQuerystring ->
            { model | indexModel = Wizard.DocumentTemplates.Index.Models.initialModel paginationQuerystring }
