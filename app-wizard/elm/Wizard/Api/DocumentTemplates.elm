module Wizard.Api.DocumentTemplates exposing
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

import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.UuidResponse as UuidResponse exposing (UuidResponse)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Utils.Bool as Bool
import File exposing (File)
import Json.Decode as D
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.Models.DocumentTemplate as DocumentTemplate exposing (DocumentTemplate)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase exposing (DocumentTemplatePhase)
import Wizard.Api.Models.DocumentTemplateAllSuggestion as DocumentTemplateAllSuggestion exposing (DocumentTemplateAllSuggestion)
import Wizard.Api.Models.DocumentTemplateDetail as DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Wizard.Api.Models.DocumentTemplateSuggestion as DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Wizard.Data.AppState as AppState exposing (AppState)


getTemplates : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination DocumentTemplate) msg -> Cmd msg
getTemplates appState _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/document-templates" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "documentTemplates" DocumentTemplate.decoder)


getTemplatesAll : AppState -> ToMsg (List DocumentTemplateAllSuggestion) msg -> Cmd msg
getTemplatesAll appState =
    Request.get (AppState.toServerInfo appState) "/document-templates/all" (D.list DocumentTemplateAllSuggestion.decoder)


getOutdatedTemplates : AppState -> ToMsg (Pagination DocumentTemplate) msg -> Cmd msg
getOutdatedTemplates appState =
    let
        queryString =
            PaginationQueryString.empty
                |> PaginationQueryString.withSize (Just 5)
                |> PaginationQueryString.toApiUrlWith [ ( "outdated", "true" ) ]

        url =
            "/document-templates" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "documentTemplates" DocumentTemplate.decoder)


getTemplate : AppState -> Uuid -> ToMsg DocumentTemplateDetail msg -> Cmd msg
getTemplate appState templateUuid =
    Request.get (AppState.toServerInfo appState) ("/document-templates/" ++ Uuid.toString templateUuid) DocumentTemplateDetail.decoder


getTemplatesFor : AppState -> Uuid -> PaginationQueryString -> ToMsg (Pagination DocumentTemplateSuggestion) msg -> Cmd msg
getTemplatesFor appState knowledgeModelPackageUuid qs =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "knowledgeModelPackageUuid", Uuid.toString knowledgeModelPackageUuid )
                , ( "phase", DocumentTemplatePhase.toString DocumentTemplatePhase.Released )
                ]
                qs

        url =
            "/document-templates/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "documentTemplates" DocumentTemplateSuggestion.decoder)


getTemplatesSuggestions : AppState -> Maybe Bool -> Bool -> PaginationQueryString -> ToMsg (Pagination DocumentTemplateSuggestion) msg -> Cmd msg
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
            "/document-templates/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "documentTemplates" DocumentTemplateSuggestion.decoder)


putTemplate : AppState -> { t | uuid : Uuid, phase : DocumentTemplatePhase } -> ToMsg DocumentTemplateDetail msg -> Cmd msg
putTemplate appState documentTemplate =
    let
        body =
            DocumentTemplateDetail.encode documentTemplate
    in
    Request.put (AppState.toServerInfo appState) ("/document-templates/" ++ Uuid.toString documentTemplate.uuid) DocumentTemplateDetail.decoder body


deleteTemplate : AppState -> String -> String -> ToMsg () msg -> Cmd msg
deleteTemplate appState organizationId templateId =
    Request.delete (AppState.toServerInfo appState) ("/document-templates/?organizationId=" ++ organizationId ++ "&templateId=" ++ templateId)


deleteTemplateVersion : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteTemplateVersion appState templateUuid =
    Request.delete (AppState.toServerInfo appState) ("/document-templates/" ++ Uuid.toString templateUuid)


pullTemplate : AppState -> String -> ToMsg UuidResponse msg -> Cmd msg
pullTemplate appState templateId =
    Request.postEmptyBody (AppState.toServerInfo appState) ("/document-templates/" ++ templateId ++ "/pull") UuidResponse.decoder


importTemplate : AppState -> File -> ToMsg () msg -> Cmd msg
importTemplate appState file =
    Request.postFile (AppState.toServerInfo appState) "/document-templates/bundle" file


exportTemplateUrl : Uuid -> String
exportTemplateUrl templateUuid =
    "/document-templates/" ++ Uuid.toString templateUuid ++ "/bundle"
