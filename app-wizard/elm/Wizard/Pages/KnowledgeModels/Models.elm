module Wizard.Pages.KnowledgeModels.Models exposing (Model, initLocalModel, initialModel)

import Shared.Data.PaginationQueryString as PaginationQueryString
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KnowledgeModels.Detail.Models
import Wizard.Pages.KnowledgeModels.Import.Models
import Wizard.Pages.KnowledgeModels.Index.Models
import Wizard.Pages.KnowledgeModels.Preview.Models
import Wizard.Pages.KnowledgeModels.ResourcePage.Models
import Wizard.Pages.KnowledgeModels.Routes exposing (Route(..))


type alias Model =
    { detailModel : Wizard.Pages.KnowledgeModels.Detail.Models.Model
    , importModel : Wizard.Pages.KnowledgeModels.Import.Models.Model
    , indexModel : Wizard.Pages.KnowledgeModels.Index.Models.Model
    , previewModel : Wizard.Pages.KnowledgeModels.Preview.Models.Model
    , resourcePageModel : Wizard.Pages.KnowledgeModels.ResourcePage.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { detailModel = Wizard.Pages.KnowledgeModels.Detail.Models.initialModel
    , importModel = Wizard.Pages.KnowledgeModels.Import.Models.initialModel appState Nothing
    , indexModel = Wizard.Pages.KnowledgeModels.Index.Models.initialModel PaginationQueryString.empty
    , previewModel = Wizard.Pages.KnowledgeModels.Preview.Models.initialModel Nothing
    , resourcePageModel = Wizard.Pages.KnowledgeModels.ResourcePage.Models.initialModel ""
    }


initLocalModel : Route -> AppState -> Model -> Model
initLocalModel route appState model =
    case route of
        DetailRoute _ ->
            { model | detailModel = Wizard.Pages.KnowledgeModels.Detail.Models.initialModel }

        ImportRoute packageId ->
            { model | importModel = Wizard.Pages.KnowledgeModels.Import.Models.initialModel appState packageId }

        IndexRoute paginationQueryString ->
            { model | indexModel = Wizard.Pages.KnowledgeModels.Index.Models.initialModel paginationQueryString }

        PreviewRoute _ mbQuestionUuid ->
            { model | previewModel = Wizard.Pages.KnowledgeModels.Preview.Models.initialModel mbQuestionUuid }

        ResourcePageRoute _ resourcePageUuid ->
            { model | resourcePageModel = Wizard.Pages.KnowledgeModels.ResourcePage.Models.initialModel resourcePageUuid }
