module Wizard.Pages.DocumentTemplates.Models exposing (Model, initLocalModel, initialModel)

import Common.Data.PaginationQueryString as PaginationQueryString
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.DocumentTemplates.Detail.Models
import Wizard.Pages.DocumentTemplates.Import.Models
import Wizard.Pages.DocumentTemplates.Index.Models
import Wizard.Pages.DocumentTemplates.Routes exposing (Route(..))


type alias Model =
    { detailModel : Wizard.Pages.DocumentTemplates.Detail.Models.Model
    , importModel : Wizard.Pages.DocumentTemplates.Import.Models.Model
    , indexModel : Wizard.Pages.DocumentTemplates.Index.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { detailModel = Wizard.Pages.DocumentTemplates.Detail.Models.initialModel
    , importModel = Wizard.Pages.DocumentTemplates.Import.Models.initialModel appState Nothing
    , indexModel = Wizard.Pages.DocumentTemplates.Index.Models.initialModel PaginationQueryString.empty
    }


initLocalModel : Route -> AppState -> Model -> Model
initLocalModel route appState model =
    case route of
        DetailRoute _ ->
            { model | detailModel = Wizard.Pages.DocumentTemplates.Detail.Models.initialModel }

        ImportRoute packageId ->
            { model | importModel = Wizard.Pages.DocumentTemplates.Import.Models.initialModel appState packageId }

        IndexRoute paginationQuerystring ->
            { model | indexModel = Wizard.Pages.DocumentTemplates.Index.Models.initialModel paginationQuerystring }
