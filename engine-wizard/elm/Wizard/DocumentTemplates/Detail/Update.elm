module Wizard.DocumentTemplates.Detail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Api.DocumentTemplates as DocumentTemplatesApi
import Shared.Data.DocumentTemplate.DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Setters exposing (setTemplate)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.FileDownloader as FileDownloader
import Wizard.DocumentTemplates.Detail.Models exposing (Model)
import Wizard.DocumentTemplates.Detail.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : String -> AppState -> Cmd Msg
fetchData templateId appState =
    DocumentTemplatesApi.getTemplate templateId appState GetTemplateCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetTemplateCompleted result ->
            applyResult appState
                { setResult = setTemplate
                , defaultError = gettext "Unable to get the document template." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                }

        ShowDeleteDialog visible ->
            ( { model | showDeleteDialog = visible, deletingVersion = Unset }, Cmd.none )

        DeleteVersion ->
            handleDeleteVersion wrapMsg appState model

        DeleteVersionCompleted result ->
            handleDeleteVersionCompleted appState model result

        UpdatePhase phase ->
            handleSetDeprecated wrapMsg appState model phase

        UpdatePhaseCompleted result ->
            applyResult appState
                { setResult = setTemplate
                , defaultError = gettext "Unable to update the document template." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                }

        ExportTemplate template ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.fetchFile appState (DocumentTemplatesApi.exportTemplateUrl template.id appState)) )

        FileDownloaderMsg fileDownloaderMsg ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg) )


handleDeleteVersion : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteVersion wrapMsg appState model =
    case model.template of
        Success template ->
            ( { model | deletingVersion = Loading }
            , Cmd.map wrapMsg <| DocumentTemplatesApi.deleteTemplateVersion template.id appState DeleteVersionCompleted
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
            , getResultCmd Wizard.Msgs.logoutMsg result
            )


handleSetDeprecated : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> DocumentTemplatePhase -> ( Model, Cmd Wizard.Msgs.Msg )
handleSetDeprecated wrapMsg appState model phase =
    case model.template of
        Success documentTemplate ->
            let
                newDocumentTemplate =
                    { documentTemplate | phase = phase }
            in
            ( model, DocumentTemplatesApi.putTemplate newDocumentTemplate appState (wrapMsg << UpdatePhaseCompleted) )

        _ ->
            ( model, Cmd.none )
