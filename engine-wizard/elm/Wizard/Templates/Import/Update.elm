module Wizard.Templates.Import.Update exposing (update)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Templates.Import.FileImport.Models as FileImportModels
import Wizard.Templates.Import.FileImport.Update as FileImportUpdate
import Wizard.Templates.Import.Models exposing (ImportModel(..), Model)
import Wizard.Templates.Import.Msgs exposing (Msg(..))
import Wizard.Templates.Import.RegistryImport.Models as RegistryImportModels
import Wizard.Templates.Import.RegistryImport.Update as RegistryImportUpdate


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

        ( ShowRegistryImport, _ ) ->
            ( { model | importModel = RegistryImportModel <| RegistryImportModels.initialModel "" }
            , Cmd.none
            )

        ( ShowFileImport, _ ) ->
            ( { model | importModel = FileImportModel FileImportModels.initialModel }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )
