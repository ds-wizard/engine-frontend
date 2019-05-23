module KnowledgeModels.Import.Update exposing (update)

import Common.AppState exposing (AppState)
import KnowledgeModels.Import.FileImport.Models as FileImportModels
import KnowledgeModels.Import.FileImport.Update as FileImportUpdate
import KnowledgeModels.Import.Models exposing (ImportModel(..), Model)
import KnowledgeModels.Import.Msgs exposing (Msg(..))
import KnowledgeModels.Import.RegistryImport.Models as RegistryImportModels
import KnowledgeModels.Import.RegistryImport.Update as RegistryImportUpdate
import Msgs


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case ( msg, model.importModel ) of
        ( FileImportMsg fileImportMsg, FileImportModel fileImportModel ) ->
            let
                ( newFileImportModel, fileImportCmd ) =
                    FileImportUpdate.update fileImportMsg (wrapMsg << FileImportMsg) appState fileImportModel
            in
            ( { model | importModel = FileImportModel newFileImportModel }
            , fileImportCmd
            )

        ( RegistryImportMsg registryImportMsg, RegistryImportModel registryImoprtModel ) ->
            let
                ( newRegistryImportModel, registryImportCmd ) =
                    RegistryImportUpdate.update registryImportMsg (wrapMsg << RegistryImportMsg) appState registryImoprtModel
            in
            ( { model | importModel = RegistryImportModel newRegistryImportModel }
            , registryImportCmd
            )

        ( ShowRegistryImport, _ ) ->
            ( { model | importModel = RegistryImportModel RegistryImportModels.initialModel }
            , Cmd.none
            )

        ( ShowFileImport, _ ) ->
            ( { model | importModel = FileImportModel FileImportModels.initialModel }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )
