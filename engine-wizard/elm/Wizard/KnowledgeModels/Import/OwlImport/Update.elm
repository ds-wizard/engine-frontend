module Wizard.KnowledgeModels.Import.OwlImport.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import File
import Form
import Gettext exposing (gettext)
import Json.Decode exposing (decodeValue)
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Utils.RequestHelpers as RequestHelpers
import Wizard.Api.Packages as PackagesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.KnowledgeModels.Common.OwlImportForm as OwlImportForm
import Wizard.KnowledgeModels.Import.OwlImport.Models exposing (Model, dropzoneId, fileInputId)
import Wizard.KnowledgeModels.Import.OwlImport.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Ports as Ports exposing (createDropzone, fileSelected)
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        DragEnter ->
            ( { model | dnd = model.dnd + 1 }, createDropzone dropzoneId )

        DragLeave ->
            ( { model | dnd = model.dnd - 1 }, Cmd.none )

        FileSelected ->
            ( model, fileSelected fileInputId )

        FileRead data ->
            case decodeValue File.decoder data of
                Ok fileData ->
                    ( { model | file = Just fileData }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        CancelFile ->
            ( { model | file = Nothing, importing = Unset, dnd = 0 }, Cmd.none )

        ImportOwlCompleted result ->
            importOwlCompleted appState model result

        Cancel ->
            ( model, Ports.historyBack (Routing.toUrl Routes.knowledgeModelsIndex) )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form, model.file ) of
                ( Form.Submit, Just owlImportForm, Just file ) ->
                    let
                        data =
                            [ ( "name", owlImportForm.name )
                            , ( "organizationId", owlImportForm.organizationId )
                            , ( "kmId", owlImportForm.kmId )
                            , ( "version", owlImportForm.version )
                            , ( "rootElement", owlImportForm.rootElement )
                            ]

                        dataWithPreviousPackageId =
                            case owlImportForm.previousPackageId of
                                Just previousPackageId ->
                                    ( "previousPackageId", previousPackageId ) :: data

                                Nothing ->
                                    data
                    in
                    ( { model | importing = Loading }
                    , Cmd.map wrapMsg <| PackagesApi.importFromOwl appState dataWithPreviousPackageId file ImportOwlCompleted
                    )

                _ ->
                    ( { model | form = Form.update OwlImportForm.validation formMsg model.form }, Cmd.none )

        _ ->
            ( model, Cmd.none )


importOwlCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
importOwlCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState Routes.knowledgeModelsIndex )

        Err error ->
            ( { model | importing = ApiError.toActionResult appState (gettext "Importing the package failed." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )
