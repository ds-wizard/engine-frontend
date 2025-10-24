module Wizard.Pages.KnowledgeModels.Import.Update exposing (update)

import Wizard.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Wizard.Components.FileImport as FileImport
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KnowledgeModels.Import.Models exposing (ImportModel(..), Model)
import Wizard.Pages.KnowledgeModels.Import.Msgs exposing (Msg(..))
import Wizard.Pages.KnowledgeModels.Import.OwlImport.Models as OwlImportModels
import Wizard.Pages.KnowledgeModels.Import.OwlImport.Update as OwlImportUpdate
import Wizard.Pages.KnowledgeModels.Import.RegistryImport.Models as RegistryImportModels
import Wizard.Pages.KnowledgeModels.Import.RegistryImport.Update as RegistryImportUpdate


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case ( msg, model.importModel ) of
        ( FileImportMsg fileImportMsg, FileImportModel fileImportModel ) ->
            let
                ( newFileImportModel, fileImportCmd ) =
                    FileImport.update
                        { mimes = [ "*/*" ]
                        , upload = KnowledgeModelPackagesApi.importKnowledgeModelPackage
                        , wrapMsg = wrapMsg << FileImportMsg
                        }
                        appState
                        fileImportMsg
                        fileImportModel
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
            ( { model | importModel = FileImportModel FileImport.initialModel }
            , Cmd.none
            )

        ( ShowOwlImport, _ ) ->
            ( { model | importModel = OwlImportModel (OwlImportModels.initialModel appState) }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )
