module KnowledgeModels.Import.Models exposing (ImportModel(..), Model, initialModel)

import Common.AppState exposing (AppState)
import Common.Config exposing (Registry(..))
import KnowledgeModels.Import.FileImport.Models as FileImportModels
import KnowledgeModels.Import.RegistryImport.Models as RegistryImportModels


type ImportModel
    = FileImportModel FileImportModels.Model
    | RegistryImportModel RegistryImportModels.Model


type alias Model =
    { importModel : ImportModel }


initialModel : AppState -> Maybe String -> Model
initialModel appState packageId =
    case appState.config.registry of
        RegistryEnabled _ ->
            { importModel = RegistryImportModel <| RegistryImportModels.initialModel <| Maybe.withDefault "" packageId }

        _ ->
            { importModel = FileImportModel <| FileImportModels.initialModel }
