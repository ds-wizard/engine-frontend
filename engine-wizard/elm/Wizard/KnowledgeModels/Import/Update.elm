module Wizard.KnowledgeModels.Import.Update exposing (update)

import Wizard.Common.AppState exposing (AppState)
import Wizard.KnowledgeModels.Import.FileImport.Models as FileImportModels
import Wizard.KnowledgeModels.Import.FileImport.Update as FileImportUpdate
import Wizard.KnowledgeModels.Import.Models exposing (ImportModel(..), Model)
import Wizard.KnowledgeModels.Import.Msgs exposing (Msg(..))
import Wizard.KnowledgeModels.Import.OwlImport.Models as OwlImportModels
import Wizard.KnowledgeModels.Import.OwlImport.Update as OwlImportUpdate
import Wizard.KnowledgeModels.Import.RegistryImport.Models as RegistryImportModels
import Wizard.KnowledgeModels.Import.RegistryImport.Update as RegistryImportUpdate
import Wizard.Msgs


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
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

        ( OwlImportMsg owlImportMsg, OwlImportModel owlImoprtModel ) ->
            let
                ( newOwlImportModel, owlImportCmd ) =
                    OwlImportUpdate.update owlImportMsg (wrapMsg << OwlImportMsg) appState owlImoprtModel
            in
            ( { model | importModel = OwlImportModel newOwlImportModel }
            , owlImportCmd
            )

        ( ShowRegistryImport, _ ) ->
            ( { model | importModel = RegistryImportModel (RegistryImportModels.initialModel "") }
            , Cmd.none
            )

        ( ShowFileImport, _ ) ->
            ( { model | importModel = FileImportModel FileImportModels.initialModel }
            , Cmd.none
            )

        ( ShowOwlImport, _ ) ->
            ( { model | importModel = OwlImportModel (OwlImportModels.initialModel appState) }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )
