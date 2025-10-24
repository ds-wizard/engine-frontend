module Wizard.Pages.DocumentTemplates.Import.Models exposing (ImportModel(..), Model, initialModel)

import Wizard.Api.Models.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Wizard.Components.FileImport as FileImport
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.DocumentTemplates.Import.RegistryImport.Models as RegistryImportModels


type ImportModel
    = FileImportModel FileImport.Model
    | RegistryImportModel RegistryImportModels.Model


type alias Model =
    { importModel : ImportModel }


initialModel : AppState -> Maybe String -> Model
initialModel appState documentTemplateId =
    case appState.config.registry of
        RegistryEnabled _ ->
            { importModel = RegistryImportModel <| RegistryImportModels.initialModel <| Maybe.withDefault "" documentTemplateId }

        _ ->
            { importModel = FileImportModel <| FileImport.initialModel }
