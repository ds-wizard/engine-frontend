module Wizard.Pages.DocumentTemplates.Detail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.FileDownloader as FileDownloader
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setTemplate)
import Gettext exposing (gettext)
import Wizard.Api.DocumentTemplates as DocumentTemplatesApi
import Wizard.Api.Models.DocumentTemplate.DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.DocumentTemplates.Detail.Models exposing (Model)
import Wizard.Pages.DocumentTemplates.Detail.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : String -> AppState -> Cmd Msg
fetchData templateId appState =
    DocumentTemplatesApi.getTemplate appState templateId GetTemplateCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetTemplateCompleted result ->
            RequestHelpers.applyResult
                { setResult = setTemplate
                , defaultError = gettext "Unable to get the document template." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }

        DropdownMsg state ->
            ( { model | dropdownState = state }, Cmd.none )

        ShowDeleteDialog visible ->
            ( { model | showDeleteDialog = visible, deletingVersion = Unset }, Cmd.none )

        DeleteVersion ->
            handleDeleteVersion wrapMsg appState model

        DeleteVersionCompleted result ->
            handleDeleteVersionCompleted appState model result

        UpdatePhase phase ->
            handleSetUpdatePhase wrapMsg appState model phase

        UpdatePhaseCompleted result ->
            RequestHelpers.applyResult
                { setResult = setTemplate
                , defaultError = gettext "Unable to update the document template." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }

        ExportTemplate template ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.fetchFile (AppState.toServerInfo appState) (DocumentTemplatesApi.exportTemplateUrl appState template.id)) )

        FileDownloaderMsg fileDownloaderMsg ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg) )

        ShowAllKms ->
            ( { model | showAllKms = True }, Cmd.none )


handleDeleteVersion : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteVersion wrapMsg appState model =
    case model.template of
        Success template ->
            ( { model | deletingVersion = Loading }
            , Cmd.map wrapMsg <| DocumentTemplatesApi.deleteTemplateVersion appState template.id DeleteVersionCompleted
            )

        _ ->
            ( model, Cmd.none )


handleDeleteVersionCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteVersionCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState Routes.documentTemplatesIndex )

        Err error ->
            ( { model | deletingVersion = ApiError.toActionResult appState (gettext "Document template could not be deleted." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )


handleSetUpdatePhase : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> DocumentTemplatePhase -> ( Model, Cmd Wizard.Msgs.Msg )
handleSetUpdatePhase wrapMsg appState model phase =
    case model.template of
        Success documentTemplate ->
            let
                newDocumentTemplate =
                    { documentTemplate | phase = phase }
            in
            ( model, DocumentTemplatesApi.putTemplate appState newDocumentTemplate (wrapMsg << UpdatePhaseCompleted) )

        _ ->
            ( model, Cmd.none )
