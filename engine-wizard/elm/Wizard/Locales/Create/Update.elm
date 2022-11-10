module Wizard.Locales.Create.Update exposing (update)

import ActionResult
import File
import Form
import Gettext exposing (gettext)
import Json.Decode exposing (decodeValue)
import Shared.Api.Locales as LocalesApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import String exposing (fromInt)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Locales.Common.LocaleCreateForm as LocaleCreateForm
import Wizard.Locales.Create.Models exposing (Model, dropzoneId, fileInputId)
import Wizard.Locales.Create.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


update : AppState -> Msg -> (Msg -> Wizard.Msgs.Msg) -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update appState msg wrapMsg model =
    case msg of
        DragEnter ->
            ( { model | dnd = model.dnd + 1 }, Ports.createLocaleDropzone dropzoneId )

        DragLeave ->
            ( { model | dnd = model.dnd - 1 }, Cmd.none )

        FileSelected ->
            ( model, Ports.localeFileSelected fileInputId )

        FileRead data ->
            case decodeValue File.decoder data of
                Ok fileData ->
                    ( { model | file = Just fileData }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        CancelFile ->
            ( { model | file = Nothing, creatingLocale = ActionResult.Unset, dnd = 0 }, Cmd.none )

        CreateCompleted result ->
            handleCreateCompleted appState model result

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model

        _ ->
            ( model, Cmd.none )


handleForm : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form, model.file ) of
        ( Form.Submit, Just form, Just file ) ->
            let
                data =
                    [ ( "name", form.name )
                    , ( "localeId", form.localeId )
                    , ( "code", form.code )
                    , ( "license", form.license )
                    , ( "description", form.description )
                    , ( "readme", form.readme )
                    , ( "version", String.join "." <| List.map fromInt [ form.localeMajor, form.localeMinor, form.localePatch ] )
                    , ( "recommendedAppVersion", String.join "." <| List.map fromInt [ form.appMajor, form.appMinor, form.appPatch ] )
                    ]

                cmd =
                    Cmd.map wrapMsg <|
                        LocalesApi.createFromPO data file appState CreateCompleted
            in
            ( { model | creatingLocale = ActionResult.Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update LocaleCreateForm.validation formMsg model.form }
            in
            ( newModel, Cmd.none )


handleCreateCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleCreateCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState Routes.localesIndex )

        Err error ->
            ( { model | creatingLocale = ApiError.toActionResult appState (gettext "Creating of the locale failed." appState.locale) error }
            , getResultCmd Wizard.Msgs.logoutMsg result
            )
