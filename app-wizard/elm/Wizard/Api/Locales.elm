module Wizard.Api.Locales exposing
    ( createFromPO
    , deleteLocale
    , deleteLocaleVersion
    , exportLocaleUrl
    , getLocale
    , getLocales
    , getLocalesSuggestions
    , importLocale
    , pullLocale
    , setDefaultLocale
    , setEnabled
    )

import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.Pagination as Pagination exposing (Pagination)
import Common.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import File exposing (File)
import Http
import Json.Decode as D
import Wizard.Api.Models.Locale as Locale exposing (Locale)
import Wizard.Api.Models.LocaleDetail as LocaleDetail exposing (LocaleDetail)
import Wizard.Api.Models.LocaleSuggestion as LocaleSuggestion exposing (LocaleSuggestion)
import Wizard.Data.AppState as AppState exposing (AppState)


getLocales : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination Locale) msg -> Cmd msg
getLocales appState _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/locales" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "locales" Locale.decoder)


getLocalesSuggestions : AppState -> ToMsg (List LocaleSuggestion) msg -> Cmd msg
getLocalesSuggestions appState =
    let
        decoder =
            D.map .items <| Pagination.decoder "locales" LocaleSuggestion.decoder
    in
    Request.get (AppState.toServerInfo appState) "/locales/suggestions" decoder


getLocale : AppState -> String -> ToMsg LocaleDetail msg -> Cmd msg
getLocale appState localeId =
    Request.get (AppState.toServerInfo appState) ("/locales/" ++ localeId) LocaleDetail.decoder


setDefaultLocale : AppState -> { b | id : String, enabled : Bool, defaultLocale : Bool } -> ToMsg () msg -> Cmd msg
setDefaultLocale appState locale =
    let
        body =
            LocaleDetail.encode { locale | defaultLocale = True }
    in
    Request.putWhatever (AppState.toServerInfo appState) ("/locales/" ++ locale.id) body


setEnabled : AppState -> { b | id : String, enabled : Bool, defaultLocale : Bool } -> Bool -> ToMsg () msg -> Cmd msg
setEnabled appState locale enabled =
    let
        body =
            LocaleDetail.encode { locale | enabled = enabled }
    in
    Request.putWhatever (AppState.toServerInfo appState) ("/locales/" ++ locale.id) body


createFromPO : AppState -> List ( String, String ) -> File -> File -> ToMsg () msg -> Cmd msg
createFromPO appState params wizardContent mailContent =
    let
        httpParams =
            Http.filePart "wizardContent" wizardContent
                :: Http.filePart "mailContent" mailContent
                :: List.map (\( k, v ) -> Http.stringPart k v) params
    in
    Request.postMultiPart (AppState.toServerInfo appState) "/locales" httpParams


deleteLocale : AppState -> String -> String -> ToMsg () msg -> Cmd msg
deleteLocale appState organizationId localeId =
    Request.delete (AppState.toServerInfo appState) ("/locales/?organizationId=" ++ organizationId ++ "&localeId=" ++ localeId)


deleteLocaleVersion : AppState -> String -> ToMsg () msg -> Cmd msg
deleteLocaleVersion appState localeId =
    Request.delete (AppState.toServerInfo appState) ("/locales/" ++ localeId)


pullLocale : AppState -> String -> ToMsg () msg -> Cmd msg
pullLocale appState localeId =
    Request.postEmpty (AppState.toServerInfo appState) ("/locales/" ++ localeId ++ "/pull")


importLocale : AppState -> File -> ToMsg () msg -> Cmd msg
importLocale appState file =
    Request.postFile (AppState.toServerInfo appState) "/locales/bundle" file


exportLocaleUrl : AppState -> String -> String
exportLocaleUrl appState localeId =
    appState.apiUrl ++ "/locales/" ++ localeId ++ "/bundle"
