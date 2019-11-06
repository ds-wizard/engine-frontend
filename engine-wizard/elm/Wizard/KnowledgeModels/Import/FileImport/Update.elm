module Wizard.KnowledgeModels.Import.FileImport.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.Api.Packages as PackagesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Locale exposing (lg)
import Wizard.KnowledgeModels.Import.FileImport.Models exposing (Model, dropzoneId, fileInputId)
import Wizard.KnowledgeModels.Import.FileImport.Msgs exposing (Msg(..))
import Wizard.KnowledgeModels.Routes exposing (Route(..))
import Wizard.Msgs
import Wizard.Ports exposing (FilePortData, createDropzone, fileSelected)
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
            ( { model | file = Just data }, Cmd.none )

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
            ( model, cmdNavigate appState (Routes.KnowledgeModelsRoute IndexRoute) )

        Err error ->
            ( { model | importing = ApiError.toActionResult (lg "apiError.packages.importError" appState) error }
            , getResultCmd result
            )
