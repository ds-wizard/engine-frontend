module Wizard.KnowledgeModels.Import.FileImport.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import File
import Json.Decode exposing (decodeValue)
import Shared.Api.Packages as PackagesApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KnowledgeModels.Import.FileImport.Models exposing (Model, dropzoneId, fileInputId)
import Wizard.KnowledgeModels.Import.FileImport.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Ports exposing (createDropzone, fileSelected)
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


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

        Submit ->
            handleSubmit wrapMsg appState model

        Cancel ->
            ( { model | file = Nothing, importing = Unset, dnd = 0 }, Cmd.none )

        ImportPackageCompleted result ->
            importPackageCompleted appState model result

        _ ->
            ( model, Cmd.none )


handleSubmit : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleSubmit wrapMsg appState model =
    case model.file of
        Just file ->
            ( { model | importing = Loading }
            , Cmd.map wrapMsg <| PackagesApi.importPackage file appState ImportPackageCompleted
            )

        Nothing ->
            ( model, Cmd.none )


importPackageCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
importPackageCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState Routes.knowledgeModelsIndex )

        Err error ->
            ( { model | importing = ApiError.toActionResult appState (lg "apiError.packages.importError" appState) error }
            , getResultCmd result
            )
