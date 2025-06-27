module Wizard.Locales.Detail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Setters exposing (setLocale)
import Shared.Utils.RequestHelpers as RequestHelpers
import Wizard.Api.Locales as LocalesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.FileDownloader as FileDownloader
import Wizard.Locales.Detail.Models exposing (Model)
import Wizard.Locales.Detail.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : String -> AppState -> Cmd Msg
fetchData localeId appState =
    LocalesApi.getLocale appState localeId GetLocaleCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetLocaleCompleted result ->
            RequestHelpers.applyResult
                { setResult = setLocale
                , defaultError = gettext "Unable to get the locale." appState.locale
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
            deleteVersionCompleted appState model result

        SetDefault ->
            case model.locale of
                Success locale ->
                    ( { model | locale = ActionResult.Loading }
                    , LocalesApi.setDefaultLocale appState locale (wrapMsg << always SetDefaultCompleted)
                    )

                _ ->
                    ( model, Cmd.none )

        SetDefaultCompleted ->
            ( model, Ports.refresh () )

        SetEnabled enabled ->
            case model.locale of
                Success locale ->
                    ( { model | locale = ActionResult.Loading }
                    , LocalesApi.setEnabled appState locale enabled (wrapMsg << always SetEnabledCompleted)
                    )

                _ ->
                    ( model, Cmd.none )

        SetEnabledCompleted ->
            ( model, Ports.refresh () )

        ExportLocale locale ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.fetchFile appState (LocalesApi.exportLocaleUrl appState locale.id)) )

        FileDownloaderMsg fileDownloaderMsg ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg) )


handleDeleteVersion : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteVersion wrapMsg appState model =
    case model.locale of
        Success locale ->
            ( { model | deletingVersion = Loading }
            , Cmd.map wrapMsg <| LocalesApi.deleteLocaleVersion appState locale.id DeleteVersionCompleted
            )

        _ ->
            ( model, Cmd.none )


deleteVersionCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
deleteVersionCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState Routes.localesIndex )

        Err error ->
            ( { model | deletingVersion = ApiError.toActionResult appState (gettext "Locale could not be deleted." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )
