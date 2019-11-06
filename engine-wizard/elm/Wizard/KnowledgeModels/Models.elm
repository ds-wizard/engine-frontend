module Wizard.KnowledgeModels.Models exposing (Model, initLocalModel, initialModel)

import Wizard.Common.AppState exposing (AppState)
import Wizard.KnowledgeModels.Detail.Models
import Wizard.KnowledgeModels.Import.Models
import Wizard.KnowledgeModels.Index.Models
import Wizard.KnowledgeModels.Routes exposing (Route(..))


type alias Model =
    { detailModel : Wizard.KnowledgeModels.Detail.Models.Model
    , importModel : Wizard.KnowledgeModels.Import.Models.Model
    , indexModel : Wizard.KnowledgeModels.Index.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { detailModel = Wizard.KnowledgeModels.Detail.Models.initialModel
    , importModel = Wizard.KnowledgeModels.Import.Models.initialModel appState Nothing
    , indexModel = Wizard.KnowledgeModels.Index.Models.initialModel
    }


initLocalModel : Route -> AppState -> Model -> Model
initLocalModel route appState model =
    case route of
        DetailRoute _ ->
            { model | detailModel = Wizard.KnowledgeModels.Detail.Models.initialModel }

        ImportRoute packageId ->
            { model | importModel = Wizard.KnowledgeModels.Import.Models.initialModel appState packageId }

        IndexRoute ->
            { model | indexModel = Wizard.KnowledgeModels.Index.Models.initialModel }
