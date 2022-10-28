module Wizard.Templates.Import.FileImport.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import File
import Gettext exposing (gettext)
import Json.Decode exposing (decodeValue)
import Shared.Api.Templates as TemplatesApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Ports exposing (createDropzone, fileSelected)
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)
import Wizard.Templates.Import.FileImport.Models exposing (Model, dropzoneId, fileInputId)
import Wizard.Templates.Import.FileImport.Msgs exposing (Msg(..))


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

        ImportTemplateCompleted result ->
            importTemplateCompleted appState model result

        _ ->
            ( model, Cmd.none )


handleSubmit : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleSubmit wrapMsg appState model =
    case model.file of
        Just file ->
            ( { model | importing = Loading }
            , Cmd.map wrapMsg <| TemplatesApi.importTemplate file appState ImportTemplateCompleted
            )

        Nothing ->
            ( model, Cmd.none )


importTemplateCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
importTemplateCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState Routes.templatesIndex )

        Err error ->
            ( { model | importing = ApiError.toActionResult appState (gettext "Importing the document template failed." appState.locale) error }
            , getResultCmd result
            )
