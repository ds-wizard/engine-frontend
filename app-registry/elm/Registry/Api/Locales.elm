module Registry.Api.Locales exposing (getLocale, getLocales)

import Common.Api.Request as Requests exposing (ToMsg)
import Json.Decode as D
import Registry.Api.Models.Locale as Locale exposing (Locale)
import Registry.Api.Models.LocaleDetail as LocaleDetail exposing (LocaleDetail)
import Registry.Data.AppState as AppState exposing (AppState)


getLocales : AppState -> ToMsg (List Locale) msg -> Cmd msg
getLocales appState =
    Requests.get (AppState.toServerInfo appState) "/locales" (D.list Locale.decoder)


getLocale : AppState -> String -> ToMsg LocaleDetail msg -> Cmd msg
getLocale appState localeId =
    Requests.get (AppState.toServerInfo appState) ("/locales/" ++ localeId) LocaleDetail.decoder
