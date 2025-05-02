module Shared.Api.Locales exposing
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

import File exposing (File)
import Http
import Json.Decode as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtDelete, jwtGet, jwtPostEmpty, jwtPostFile, jwtPostMultiPart, jwtPut)
import Shared.Data.Locale as Locale exposing (Locale)
import Shared.Data.LocaleDetail as LocaleDetail exposing (LocaleDetail)
import Shared.Data.LocaleSuggestion as LocaleSuggestion exposing (LocaleSuggestion)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)


getLocales : PaginationQueryFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination Locale) msg -> Cmd msg
getLocales _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/locales" ++ queryString
    in
    jwtGet url (Pagination.decoder "locales" Locale.decoder)


getLocalesSuggestions : AbstractAppState a -> ToMsg (List LocaleSuggestion) msg -> Cmd msg
getLocalesSuggestions =
    let
        decoder =
            D.map .items <|
                Pagination.decoder "locales" LocaleSuggestion.decoder
    in
    jwtGet "/locales/suggestions" decoder


getLocale : String -> AbstractAppState a -> ToMsg LocaleDetail msg -> Cmd msg
getLocale localeId =
    jwtGet ("/locales/" ++ localeId) LocaleDetail.decoder


setDefaultLocale : { b | id : String, enabled : Bool, defaultLocale : Bool } -> AbstractAppState a -> ToMsg () msg -> Cmd msg
setDefaultLocale locale =
    let
        withDefault =
            LocaleDetail.encode { locale | defaultLocale = True }
    in
    jwtPut ("/locales/" ++ locale.id) withDefault


setEnabled : { b | id : String, enabled : Bool, defaultLocale : Bool } -> Bool -> AbstractAppState a -> ToMsg () msg -> Cmd msg
setEnabled locale enabled =
    let
        withDefault =
            LocaleDetail.encode { locale | enabled = enabled }
    in
    jwtPut ("/locales/" ++ locale.id) withDefault


createFromPO : List ( String, String ) -> File -> File -> AbstractAppState a -> ToMsg () msg -> Cmd msg
createFromPO params wizardContent mailContent =
    let
        httpParams =
            Http.filePart "wizardContent" wizardContent
                :: Http.filePart "mailContent" mailContent
                :: List.map (\( k, v ) -> Http.stringPart k v) params
    in
    jwtPostMultiPart "/locales" httpParams


deleteLocale : String -> String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteLocale organizationId localeId =
    jwtDelete ("/locales/?organizationId=" ++ organizationId ++ "&localeId=" ++ localeId)


deleteLocaleVersion : String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteLocaleVersion templateId =
    jwtDelete ("/locales/" ++ templateId)


pullLocale : String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
pullLocale localeId =
    jwtPostEmpty ("/locales/" ++ localeId ++ "/pull")


importLocale : File -> AbstractAppState a -> ToMsg () msg -> Cmd msg
importLocale =
    jwtPostFile "/locales/bundle"


exportLocaleUrl : String -> AbstractAppState a -> String
exportLocaleUrl localeId appState =
    appState.apiUrl ++ "/locales/" ++ localeId ++ "/bundle"
