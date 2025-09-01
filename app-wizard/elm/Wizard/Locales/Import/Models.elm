module Wizard.Locales.Import.Models exposing (ImportModel(..), Model, initialModel)

import Wizard.Api.Models.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.FileImport as FileImport
import Wizard.Locales.Import.RegistryImport.Models as RegistryImportModels


type ImportModel
    = FileImportModel FileImport.Model
    | RegistryImportModel RegistryImportModels.Model


type alias Model =
    { importModel : ImportModel }


initialModel : AppState -> Maybe String -> Model
initialModel appState packageId =
    case appState.config.registry of
        RegistryEnabled _ ->
            { importModel = RegistryImportModel <| RegistryImportModels.initialModel <| Maybe.withDefault "" packageId }

        _ ->
            { importModel = FileImportModel <| FileImport.initialModel }
