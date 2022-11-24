module Wizard.Locales.Detail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Api.Locales as LocalesApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Setters exposing (setLocale)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Locales.Detail.Models exposing (Model)
import Wizard.Locales.Detail.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : String -> AppState -> Cmd Msg
fetchData localeId appState =
    LocalesApi.getLocale localeId appState GetLocaleCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetLocaleCompleted result ->
            applyResult appState
                { setResult = setLocale
                , defaultError = gettext "Unable to get the locale." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                }

        ShowDeleteDialog visible ->
            ( { model | showDeleteDialog = visible, deletingVersion = Unset }, Cmd.none )

        DeleteVersion ->
            handleDeleteVersion wrapMsg appState model

        DeleteVersionCompleted result ->
            deleteVersionCompleted appState model result

        ExportLocale locale ->
            ( model, Ports.downloadFile (LocalesApi.exportLocaleUrl locale.id appState) )

        SetDefault ->
            case model.locale of
                Success locale ->
                    ( { model | locale = ActionResult.Loading }
                    , LocalesApi.setDefaultLocale locale appState (wrapMsg << always SetDefaultCompleted)
                    )

                _ ->
                    ( model, Cmd.none )

        SetDefaultCompleted ->
            ( model, LocalesApi.getLocale model.id appState (wrapMsg << GetLocaleCompleted) )

        SetEnabled enabled ->
            case model.locale of
                Success locale ->
                    ( { model | locale = ActionResult.Loading }
                    , LocalesApi.setEnabled locale enabled appState (wrapMsg << always SetEnabledCompleted)
                    )

                _ ->
                    ( model, Cmd.none )

        SetEnabledCompleted ->
            ( model, LocalesApi.getLocale model.id appState (wrapMsg << GetLocaleCompleted) )


handleDeleteVersion : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteVersion wrapMsg appState model =
    case model.locale of
        Success locale ->
            ( { model | deletingVersion = Loading }
            , Cmd.map wrapMsg <| LocalesApi.deleteLocaleVersion locale.id appState DeleteVersionCompleted
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
            , getResultCmd Wizard.Msgs.logoutMsg result
            )
