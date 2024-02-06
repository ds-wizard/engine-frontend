module Registry2.Api.Locales exposing (getLocale, getLocales)

import Json.Decode as D
import Registry2.Api.Models.Locale as Locale exposing (Locale)
import Registry2.Api.Models.LocaleDetail as LocaleDetail exposing (LocaleDetail)
import Registry2.Api.Requests as Requests
import Registry2.Data.AppState exposing (AppState)
import Shared.Api exposing (ToMsg)


getLocales : AppState -> ToMsg (List Locale) msg -> Cmd msg
getLocales appState =
    Requests.get appState "/locales" (D.list Locale.decoder)


getLocale : AppState -> String -> ToMsg LocaleDetail msg -> Cmd msg
getLocale appState localeId =
    Requests.get appState ("/locales/" ++ localeId) LocaleDetail.decoder
