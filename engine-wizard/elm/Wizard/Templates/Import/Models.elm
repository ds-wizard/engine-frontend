module Wizard.Templates.Import.Models exposing (ImportModel(..), Model, initialModel)

import Shared.Data.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Wizard.Common.AppState exposing (AppState)
import Wizard.Templates.Import.FileImport.Models as FileImportModels
import Wizard.Templates.Import.RegistryImport.Models as RegistryImportModels


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
