module KnowledgeModels.Models exposing (Model, initLocalModel, initialModel)

import Common.AppState exposing (AppState)
import KnowledgeModels.Detail.Models
import KnowledgeModels.Import.Models
import KnowledgeModels.Index.Models
import KnowledgeModels.Routes exposing (Route(..))


type alias Model =
    { detailModel : KnowledgeModels.Detail.Models.Model
    , importModel : KnowledgeModels.Import.Models.Model
    , indexModel : KnowledgeModels.Index.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { detailModel = KnowledgeModels.Detail.Models.initialModel
    , importModel = KnowledgeModels.Import.Models.initialModel appState Nothing
    , indexModel = KnowledgeModels.Index.Models.initialModel
    }


initLocalModel : Route -> AppState -> Model -> Model
initLocalModel route appState model =
    case route of
        DetailRoute _ ->
            { model | detailModel = KnowledgeModels.Detail.Models.initialModel }

        ImportRoute packageId ->
            { model | importModel = KnowledgeModels.Import.Models.initialModel appState packageId }

        IndexRoute ->
            { model | indexModel = KnowledgeModels.Index.Models.initialModel }
