module Wizard.KnowledgeModels.Import.Models exposing (ImportModel(..), Model, initialModel, isFileImportModel, isOwlImportModel, isRegistryImportModel)

import Shared.Data.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Wizard.Common.AppState exposing (AppState)
import Wizard.KnowledgeModels.Import.FileImport.Models as FileImportModels
import Wizard.KnowledgeModels.Import.OwlImport.Models as OwlImportModels
import Wizard.KnowledgeModels.Import.RegistryImport.Models as RegistryImportModels


type ImportModel
    = FileImportModel FileImportModels.Model
    | RegistryImportModel RegistryImportModels.Model
    | OwlImportModel OwlImportModels.Model


type alias Model =
    { importModel : ImportModel }


initialModel : AppState -> Maybe String -> Model
initialModel appState packageId =
    if appState.config.experimental.owl.enabled then
        { importModel = OwlImportModel <| OwlImportModels.initialModel appState }

    else
        case appState.config.registry of
            RegistryEnabled _ ->
                { importModel = RegistryImportModel <| RegistryImportModels.initialModel <| Maybe.withDefault "" packageId }

            _ ->
                { importModel = FileImportModel <| FileImportModels.initialModel }


isFileImportModel : Model -> Bool
isFileImportModel model =
    case model.importModel of
        FileImportModel _ ->
            True

        _ ->
            False


isRegistryImportModel : Model -> Bool
isRegistryImportModel model =
    case model.importModel of
        RegistryImportModel _ ->
            True

        _ ->
            False


isOwlImportModel : Model -> Bool
isOwlImportModel model =
    case model.importModel of
        OwlImportModel _ ->
            True

        _ ->
            False
