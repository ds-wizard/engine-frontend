module Wizard.KnowledgeModels.Import.Models exposing (ImportModel(..), Model, initialModel)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Config.KnowledgeModelRegistryConfig exposing (KnowledgeModelRegistryConfig(..))
import Wizard.KnowledgeModels.Import.FileImport.Models as FileImportModels
import Wizard.KnowledgeModels.Import.RegistryImport.Models as RegistryImportModels


type ImportModel
    = FileImportModel FileImportModels.Model
    | RegistryImportModel RegistryImportModels.Model


type alias Model =
    { importModel : ImportModel }


initialModel : AppState -> Maybe String -> Model
initialModel appState packageId =
    case appState.config.knowledgeModelRegistry of
        KnowledgeModelRegistryEnabled _ ->
            { importModel = RegistryImportModel <| RegistryImportModels.initialModel <| Maybe.withDefault "" packageId }

        _ ->
            { importModel = FileImportModel <| FileImportModels.initialModel }
