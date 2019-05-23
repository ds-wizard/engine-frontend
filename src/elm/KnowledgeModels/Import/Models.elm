module KnowledgeModels.Import.Models exposing (ImportModel(..), Model, initialModel)

import KnowledgeModels.Import.FileImport.Models as FileImportModels
import KnowledgeModels.Import.RegistryImport.Models as RegistryImportModels


type ImportModel
    = FileImportModel FileImportModels.Model
    | RegistryImportModel RegistryImportModels.Model


type alias Model =
    { importModel : ImportModel }


initialModel : Maybe String -> Model
initialModel packageId =
    { importModel = RegistryImportModel <| RegistryImportModels.initialModel <| Maybe.withDefault "" packageId }
