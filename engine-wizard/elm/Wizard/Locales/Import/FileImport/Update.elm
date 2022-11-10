module Wizard.Locales.Import.FileImport.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import File
import Gettext exposing (gettext)
import Json.Decode exposing (decodeValue)
import Shared.Api.Locales as LocalesApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Locales.Import.FileImport.Models exposing (Model, dropzoneId, fileInputId)
import Wizard.Locales.Import.FileImport.Msgs exposing (Msg(..))
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

        ImportLocaleComplete result ->
            importLocaleComplete appState model result

        _ ->
            ( model, Cmd.none )


handleSubmit : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleSubmit wrapMsg appState model =
    case model.file of
        Just file ->
            ( { model | importing = Loading }
            , Cmd.map wrapMsg <| LocalesApi.importLocale file appState ImportLocaleComplete
            )

        Nothing ->
            ( model, Cmd.none )


importLocaleComplete : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
importLocaleComplete appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState Routes.localesIndex )

        Err error ->
            ( { model | importing = ApiError.toActionResult appState (gettext "Importing the locale failed." appState.locale) error }
            , getResultCmd Wizard.Msgs.logoutMsg result
            )
