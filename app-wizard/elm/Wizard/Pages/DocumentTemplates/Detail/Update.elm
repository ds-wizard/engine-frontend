module Wizard.Pages.DocumentTemplates.Detail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Browser.Navigation as Navigation
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.FileDownloader as FileDownloader
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setTemplate)
import File.Download as Download
import Gettext exposing (gettext)
import Uuid exposing (Uuid)
import Wizard.Api.DocumentTemplates as DocumentTemplatesApi
import Wizard.Api.Models.DocumentTemplate.DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.DocumentTemplates.Detail.DocumentTemplateDetailRoute as DocumentTemplateDetailRoute exposing (DocumentTemplateDetailRoute)
import Wizard.Pages.DocumentTemplates.Detail.ImportLocaleModal as ImportLocaleModal
import Wizard.Pages.DocumentTemplates.Detail.Models exposing (Model)
import Wizard.Pages.DocumentTemplates.Detail.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate, toUrl)


fetchData : Uuid -> AppState -> Cmd Msg
fetchData templateUuid appState =
    DocumentTemplatesApi.getTemplate appState templateUuid GetTemplateCompleted


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

        OpenDetailRoute detailRoute ->
            let
                replaceUrlCmd =
                    ActionResult.unwrap Cmd.none
                        (\template -> Navigation.replaceUrl appState.key (toUrl (detailRouteToRoute detailRoute template.uuid)))
                        model.template
            in
            ( { model | detailRoute = detailRoute }, replaceUrlCmd )

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
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.fetchFile (AppState.toServerInfo appState) (DocumentTemplatesApi.exportTemplateUrl template.uuid)) )

        ExportTemplatePot template ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.fetchFile (AppState.toServerInfo appState) (DocumentTemplatesApi.exportTemplatePotUrl template.uuid)) )

        OpenImportLocaleModal ->
            case model.template of
                Success template ->
                    ( { model | importLocaleModal = ImportLocaleModal.open template.uuid }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ImportLocaleModalMsg importLocaleModalMsg ->
            let
                ( importLocaleModal, importLocaleModalCmd, mbImportedLocale ) =
                    ImportLocaleModal.update appState importLocaleModalMsg model.importLocaleModal

                template =
                    case mbImportedLocale of
                        Just locale ->
                            ActionResult.map (\t -> { t | locales = t.locales ++ [ locale ] }) model.template

                        Nothing ->
                            model.template
            in
            ( { model | importLocaleModal = importLocaleModal, template = template }
            , Cmd.map (wrapMsg << ImportLocaleModalMsg) importLocaleModalCmd
            )

        ShowDeleteLocale locale ->
            ( { model | localeToDelete = Just locale, deletingLocale = Unset }, Cmd.none )

        HideDeleteLocale ->
            ( { model | localeToDelete = Nothing }, Cmd.none )

        DeleteLocale ->
            case ( model.template, model.localeToDelete ) of
                ( Success template, Just locale ) ->
                    ( { model | deletingLocale = Loading }
                    , Cmd.map wrapMsg (DocumentTemplatesApi.deleteLocale appState template.uuid locale.uuid DeleteLocaleCompleted)
                    )

                _ ->
                    ( model, Cmd.none )

        DeleteLocaleCompleted result ->
            case result of
                Ok _ ->
                    let
                        template =
                            case model.localeToDelete of
                                Just locale ->
                                    ActionResult.map (\t -> { t | locales = List.filter (\l -> l.uuid /= locale.uuid) t.locales }) model.template

                                Nothing ->
                                    model.template
                    in
                    ( { model | localeToDelete = Nothing, deletingLocale = Success "", template = template }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | deletingLocale = ApiError.toActionResult appState (gettext "Deleting the locale failed." appState.locale) error }
                    , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
                    )

        DownloadLocale locale ->
            case model.template of
                Success template ->
                    ( model
                    , Cmd.map wrapMsg (DocumentTemplatesApi.getLocaleContent appState template.uuid locale.uuid (DownloadLocaleCompleted locale))
                    )

                _ ->
                    ( model, Cmd.none )

        DownloadLocaleCompleted locale result ->
            case result of
                Ok content ->
                    ( model, Download.string (locale.code ++ ".po") "text/x-gettext-translation" content )

                Err _ ->
                    ( model, RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result )

        FileDownloaderMsg fileDownloaderMsg ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg) )

        ShowAllKms ->
            ( { model | showAllKms = True }, Cmd.none )


detailRouteToRoute : DocumentTemplateDetailRoute -> Uuid -> Routes.Route
detailRouteToRoute detailRoute uuid =
    case detailRoute of
        DocumentTemplateDetailRoute.Readme ->
            Routes.documentTemplatesDetail uuid

        DocumentTemplateDetailRoute.Locales ->
            Routes.documentTemplatesDetailLocales uuid


handleDeleteVersion : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteVersion wrapMsg appState model =
    case model.template of
        Success template ->
            ( { model | deletingVersion = Loading }
            , Cmd.map wrapMsg <| DocumentTemplatesApi.deleteTemplateVersion appState template.uuid DeleteVersionCompleted
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
