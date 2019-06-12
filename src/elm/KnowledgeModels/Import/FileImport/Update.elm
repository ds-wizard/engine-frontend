module KnowledgeModels.Import.FileImport.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Packages as PackagesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import KnowledgeModels.Import.FileImport.Models exposing (Model, dropzoneId, fileInputId)
import KnowledgeModels.Import.FileImport.Msgs exposing (Msg(..))
import KnowledgeModels.Routing
import Msgs
import Ports exposing (FilePortData, createDropzone, fileSelected)
import Routing exposing (Route(..), cmdNavigate)


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
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


handleSubmit : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleSubmit wrapMsg appState model =
    case model.file of
        Just file ->
            ( { model | importing = Loading }
            , Cmd.map wrapMsg <| PackagesApi.importPackage file appState ImportPackageCompleted
            )

        Nothing ->
            ( model, Cmd.none )


importPackageCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
importPackageCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState.key (KnowledgeModels KnowledgeModels.Routing.Index) )

        Err error ->
            ( { model | importing = getServerError error "Importing package failed." }
            , getResultCmd result
            )
