module Wizard.Pages.KnowledgeModels.Import.Models exposing
    ( ImportModel(..)
    , Model
    , initialModel
    , isFileImportModel
    , isOwlImportModel
    , isRegistryImportModel
    )

import Wizard.Api.Models.BootstrapConfig.RegistryConfig exposing (RegistryConfig(..))
import Wizard.Components.FileImport as FileImport
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KnowledgeModels.Import.OwlImport.Models as OwlImportModels
import Wizard.Pages.KnowledgeModels.Import.RegistryImport.Models as RegistryImportModels


type ImportModel
    = FileImportModel FileImport.Model
    | RegistryImportModel RegistryImportModels.Model
    | OwlImportModel OwlImportModels.Model


type alias Model =
    { importModel : ImportModel }


initialModel : AppState -> Maybe String -> Model
initialModel appState packageId =
    case appState.config.registry of
        RegistryEnabled _ ->
            { importModel = RegistryImportModel <| RegistryImportModels.initialModel <| Maybe.withDefault "" packageId }

        _ ->
            { importModel = FileImportModel <| FileImport.initialModel }


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
