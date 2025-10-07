module Wizard.Pages.DocumentTemplates.Import.Update exposing (update)

import Wizard.Api.DocumentTemplates as DocumentTemplatesApi
import Wizard.Components.FileImport as FileImport
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.DocumentTemplates.Import.Models exposing (ImportModel(..), Model)
import Wizard.Pages.DocumentTemplates.Import.Msgs exposing (Msg(..))
import Wizard.Pages.DocumentTemplates.Import.RegistryImport.Models as RegistryImportModels
import Wizard.Pages.DocumentTemplates.Import.RegistryImport.Update as RegistryImportUpdate


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case ( msg, model.importModel ) of
        ( FileImportMsg fileImportMsg, FileImportModel fileImportModel ) ->
            let
                ( newFileImportModel, fileImportCmd ) =
                    FileImport.update
                        { mimes = [ ".zip" ]
                        , upload = DocumentTemplatesApi.importTemplate
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

        ( ShowRegistryImport, _ ) ->
            ( { model | importModel = RegistryImportModel <| RegistryImportModels.initialModel "" }
            , Cmd.none
            )

        ( ShowFileImport, _ ) ->
            ( { model | importModel = FileImportModel FileImport.initialModel }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )
