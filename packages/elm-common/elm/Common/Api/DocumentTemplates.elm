module Common.Api.DocumentTemplates exposing
    ( deleteLocale
    , deleteTemplate
    , exportTemplatePotUrl
    , exportTemplateUrl
    , getLocaleContent
    , getOutdatedTemplates
    , getTemplate
    , getTemplateDependents
    , getTemplates
    , getTemplatesAll
    , getTemplatesFor
    , getTemplatesSuggestions
    , importLocale
    , importTemplate
    , pullTemplate
    , putTemplate
    )

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.DocumentTemplate as DocumentTemplate exposing (DocumentTemplate)
import Common.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Common.Api.Models.DocumentTemplateAllSuggestion as DocumentTemplateAllSuggestion exposing (DocumentTemplateAllSuggestion)
import Common.Api.Models.DocumentTemplateDetail as DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Common.Api.Models.DocumentTemplateLocale as DocumentTemplateLocale exposing (DocumentTemplateLocale)
import Common.Api.Models.DocumentTemplateSuggestion as DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Common.Api.Models.LibraryDependents as LibraryDependents exposing (LibraryDependents)
import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.UuidResponse as UuidResponse exposing (UuidResponse)
import Common.Api.Params as Params
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Utils.Bool as Bool
import File exposing (File)
import Http
import Json.Decode as D
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)


getTemplates : Context ctx -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination DocumentTemplate) msg -> Cmd msg
getTemplates appState _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            Params.apply appState "GET" ("/document-templates" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "documentTemplates" DocumentTemplate.decoder)


getTemplatesAll : Context ctx -> ToMsg (List DocumentTemplateAllSuggestion) msg -> Cmd msg
getTemplatesAll appState =
    Request.get (Context.toServerInfo appState) (Params.apply appState "GET" "/document-templates/all") (D.list DocumentTemplateAllSuggestion.decoder)


getOutdatedTemplates : Context ctx -> ToMsg (Pagination DocumentTemplate) msg -> Cmd msg
getOutdatedTemplates appState =
    let
        queryString =
            PaginationQueryString.empty
                |> PaginationQueryString.withSize (Just 5)
                |> PaginationQueryString.toApiUrlWith [ ( "outdated", "true" ) ]

        url =
            Params.apply appState "GET" ("/document-templates" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "documentTemplates" DocumentTemplate.decoder)


getTemplate : Context ctx -> Uuid -> ToMsg DocumentTemplateDetail msg -> Cmd msg
getTemplate appState templateUuid =
    Request.get (Context.toServerInfo appState) ("/document-templates/" ++ Uuid.toString templateUuid) DocumentTemplateDetail.decoder


getTemplatesFor : Context ctx -> Uuid -> PaginationQueryString -> ToMsg (Pagination DocumentTemplateSuggestion) msg -> Cmd msg
getTemplatesFor appState knowledgeModelPackageUuid qs =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "knowledgeModelPackageUuid", Uuid.toString knowledgeModelPackageUuid )
                , ( "phase", DocumentTemplatePhase.toString DocumentTemplatePhase.Released )
                ]
                qs

        url =
            Params.apply appState "GET" ("/document-templates/suggestions" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "documentTemplates" DocumentTemplateSuggestion.decoder)


getTemplatesSuggestions : Context ctx -> Maybe Bool -> Bool -> PaginationQueryString -> ToMsg (Pagination DocumentTemplateSuggestion) msg -> Cmd msg
getTemplatesSuggestions appState nonEditable includeUnsupportedMetamodelVersion qs =
    let
        includeUnsupportedMetamodelVersionValue =
            if includeUnsupportedMetamodelVersion then
                "true"

            else
                ""

        params =
            [ ( "phase", DocumentTemplatePhase.toString DocumentTemplatePhase.Released )
            , ( "includeUnsupportedMetamodelVersion", includeUnsupportedMetamodelVersionValue )
            , ( "nonEditable", Maybe.unwrap "" Bool.toString nonEditable )
            ]

        queryString =
            PaginationQueryString.toApiUrlWith params qs

        url =
            Params.apply appState "GET" ("/document-templates/suggestions" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "documentTemplates" DocumentTemplateSuggestion.decoder)


putTemplate : Context ctx -> { t | uuid : Uuid, phase : DocumentTemplatePhase } -> ToMsg DocumentTemplateDetail msg -> Cmd msg
putTemplate appState documentTemplate =
    let
        body =
            DocumentTemplateDetail.encode documentTemplate
    in
    Request.put (Context.toServerInfo appState) ("/document-templates/" ++ Uuid.toString documentTemplate.uuid) DocumentTemplateDetail.decoder body


getTemplateDependents : Context ctx -> Uuid -> Bool -> ToMsg LibraryDependents msg -> Cmd msg
getTemplateDependents appState templateUuid allVersions =
    Request.get (Context.toServerInfo appState) ("/document-templates/" ++ Uuid.toString templateUuid ++ "/dependents?allVersions=" ++ Bool.toString allVersions) LibraryDependents.decoder


deleteTemplate : Context ctx -> Uuid -> Bool -> ToMsg () msg -> Cmd msg
deleteTemplate appState templateUuid allVersions =
    Request.delete (Context.toServerInfo appState) ("/document-templates/" ++ Uuid.toString templateUuid ++ "?allVersions=" ++ Bool.toString allVersions)


pullTemplate : Context ctx -> String -> ToMsg UuidResponse msg -> Cmd msg
pullTemplate appState templateId =
    Request.postEmptyBody (Context.toServerInfo appState) (Params.apply appState "POST" ("/document-templates/" ++ templateId ++ "/pull")) UuidResponse.decoder


importTemplate : Context ctx -> File -> ToMsg () msg -> Cmd msg
importTemplate appState file =
    Request.postFile (Context.toServerInfo appState) (Params.apply appState "POST" "/document-templates/bundle") file


exportTemplateUrl : Uuid -> String
exportTemplateUrl templateUuid =
    "/document-templates/" ++ Uuid.toString templateUuid ++ "/bundle"


exportTemplatePotUrl : Uuid -> String
exportTemplatePotUrl templateUuid =
    "/document-templates/" ++ Uuid.toString templateUuid ++ "/locales/template"


deleteLocale : Context ctx -> Uuid -> Uuid -> ToMsg () msg -> Cmd msg
deleteLocale appState templateUuid localeUuid =
    Request.delete (Context.toServerInfo appState) ("/document-templates/" ++ Uuid.toString templateUuid ++ "/locales/" ++ Uuid.toString localeUuid)


getLocaleContent : Context ctx -> Uuid -> Uuid -> ToMsg String msg -> Cmd msg
getLocaleContent appState templateUuid localeUuid =
    Request.getString (Context.toServerInfo appState) ("/document-templates/" ++ Uuid.toString templateUuid ++ "/locales/" ++ Uuid.toString localeUuid ++ "/content")


importLocale : Context ctx -> Uuid -> String -> File -> ToMsg DocumentTemplateLocale msg -> Cmd msg
importLocale appState templateUuid name poContent =
    let
        parts =
            [ Http.stringPart "name" name
            , Http.filePart "poContent" poContent
            ]
    in
    Request.postMultiPartWithData (Context.toServerInfo appState) ("/document-templates/" ++ Uuid.toString templateUuid ++ "/locales") parts DocumentTemplateLocale.decoder
