module Shared.Api.DocumentTemplates exposing
    ( deleteTemplate
    , deleteTemplateVersion
    , exportTemplateUrl
    , getOutdatedTemplates
    , getTemplate
    , getTemplates
    , getTemplatesAll
    , getTemplatesFor
    , getTemplatesSuggestions
    , importTemplate
    , pullTemplate
    , putTemplate
    )

import File exposing (File)
import Json.Decode as D
import Maybe.Extra as Maybe
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtDelete, jwtFetchPut, jwtGet, jwtOrHttpGet, jwtPostEmpty, jwtPostFile)
import Shared.Data.DocumentTemplate as DocumentTemplate exposing (DocumentTemplate)
import Shared.Data.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Shared.Data.DocumentTemplateDetail as DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Shared.Data.DocumentTemplateSuggestion as DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Utils exposing (boolToString)


getTemplates : PaginationQueryFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination DocumentTemplate) msg -> Cmd msg
getTemplates _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/document-templates" ++ queryString
    in
    jwtGet url (Pagination.decoder "documentTemplates" DocumentTemplate.decoder)


getTemplatesAll : AbstractAppState a -> ToMsg (List DocumentTemplateSuggestion) msg -> Cmd msg
getTemplatesAll =
    jwtGet "/document-templates/all" (D.list DocumentTemplateSuggestion.decoder)


getOutdatedTemplates : AbstractAppState a -> ToMsg (Pagination DocumentTemplate) msg -> Cmd msg
getOutdatedTemplates =
    let
        queryString =
            PaginationQueryString.empty
                |> PaginationQueryString.withSize (Just 5)
                |> PaginationQueryString.toApiUrlWith [ ( "state", "OutdatedTemplateState" ) ]

        url =
            "/document-templates" ++ queryString
    in
    jwtGet url (Pagination.decoder "documentTemplates" DocumentTemplate.decoder)


getTemplate : String -> AbstractAppState a -> ToMsg DocumentTemplateDetail msg -> Cmd msg
getTemplate templateId =
    jwtOrHttpGet ("/document-templates/" ++ templateId) DocumentTemplateDetail.decoder


getTemplatesFor : String -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination DocumentTemplateSuggestion) msg -> Cmd msg
getTemplatesFor pkgId qs =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "pkgId", pkgId )
                , ( "phase", DocumentTemplatePhase.toString DocumentTemplatePhase.Released )
                ]
                qs

        url =
            "/document-templates/suggestions" ++ queryString
    in
    jwtGet url (Pagination.decoder "documentTemplates" DocumentTemplateSuggestion.decoder)


getTemplatesSuggestions : Maybe Bool -> Bool -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination DocumentTemplateSuggestion) msg -> Cmd msg
getTemplatesSuggestions nonEditable includeUnsupportedMetamodelVersion qs =
    let
        includeUnsupportedMetamodelVersionValue =
            if includeUnsupportedMetamodelVersion then
                "true"

            else
                ""

        params =
            [ ( "phase", DocumentTemplatePhase.toString DocumentTemplatePhase.Released )
            , ( "includeUnsupportedMetamodelVersion", includeUnsupportedMetamodelVersionValue )
            , ( "nonEditable", Maybe.unwrap "" boolToString nonEditable )
            ]

        queryString =
            PaginationQueryString.toApiUrlWith params qs

        url =
            "/document-templates/suggestions" ++ queryString
    in
    jwtGet url (Pagination.decoder "documentTemplates" DocumentTemplateSuggestion.decoder)


putTemplate : { t | id : String, phase : DocumentTemplatePhase } -> AbstractAppState a -> ToMsg DocumentTemplateDetail msg -> Cmd msg
putTemplate documentTemplate =
    let
        body =
            DocumentTemplateDetail.encode documentTemplate
    in
    jwtFetchPut ("/document-templates/" ++ documentTemplate.id) DocumentTemplateDetail.decoder body


deleteTemplate : String -> String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteTemplate organizationId templateId =
    jwtDelete ("/document-templates/?organizationId=" ++ organizationId ++ "&templateId=" ++ templateId)


deleteTemplateVersion : String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteTemplateVersion templateId =
    jwtDelete ("/document-templates/" ++ templateId)


pullTemplate : String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
pullTemplate templateId =
    jwtPostEmpty ("/document-templates/" ++ templateId ++ "/pull")


importTemplate : File -> AbstractAppState a -> ToMsg () msg -> Cmd msg
importTemplate =
    jwtPostFile "/document-templates/bundle"


exportTemplateUrl : String -> AbstractAppState a -> String
exportTemplateUrl templateId appState =
    appState.apiUrl ++ "/document-templates/" ++ templateId ++ "/bundle"
