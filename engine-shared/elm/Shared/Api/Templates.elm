module Shared.Api.Templates exposing
    ( deleteTemplate
    , deleteTemplateVersion
    , exportTemplateUrl
    , getTemplate
    , getTemplates
    , getTemplatesAll
    , getTemplatesFor
    , importTemplate
    , pullTemplate
    )

import File exposing (File)
import Json.Decode as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, authorizedUrl, jwtDelete, jwtGet, jwtOrHttpGet, jwtPostEmpty, jwtPostFile)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.Template as Template exposing (Template)
import Shared.Data.TemplateDetail as TemplateDetail exposing (TemplateDetail)
import Shared.Data.TemplateSuggestion as TemplateSuggestion exposing (TemplateSuggestion)


getTemplates : PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination Template) msg -> Cmd msg
getTemplates qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/templates" ++ queryString
    in
    jwtGet url (Pagination.decoder "templates" Template.decoder)


getTemplatesAll : AbstractAppState a -> ToMsg (List TemplateSuggestion) msg -> Cmd msg
getTemplatesAll =
    jwtGet "/templates/all" (D.list TemplateSuggestion.decoder)


getTemplate : String -> AbstractAppState a -> ToMsg TemplateDetail msg -> Cmd msg
getTemplate templateId =
    jwtOrHttpGet ("/templates/" ++ templateId) TemplateDetail.decoder


getTemplatesFor : String -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination TemplateSuggestion) msg -> Cmd msg
getTemplatesFor pkgId qs =
    let
        queryString =
            PaginationQueryString.toApiUrlWith [ ( "pkgId", pkgId ) ] qs

        url =
            "/templates/suggestions" ++ queryString
    in
    jwtGet url (Pagination.decoder "templates" TemplateSuggestion.decoder)


deleteTemplate : String -> String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteTemplate organizationId templateId =
    jwtDelete ("/templates/?organizationId=" ++ organizationId ++ "&templateId=" ++ templateId)


deleteTemplateVersion : String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteTemplateVersion templateId =
    jwtDelete ("/templates/" ++ templateId)


pullTemplate : String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
pullTemplate templateId =
    jwtPostEmpty ("/templates/" ++ templateId ++ "/pull")


importTemplate : File -> AbstractAppState a -> ToMsg () msg -> Cmd msg
importTemplate =
    jwtPostFile "/templates/bundle"


exportTemplateUrl : String -> AbstractAppState a -> String
exportTemplateUrl templateId =
    authorizedUrl ("/templates/" ++ templateId ++ "/bundle")
