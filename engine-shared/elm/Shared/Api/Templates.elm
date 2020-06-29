module Shared.Api.Templates exposing
    ( deleteTemplate
    , deleteTemplateVersion
    , exportTemplateUrl
    , getTemplate
    , getTemplates
    , getTemplatesFor
    , importTemplate
    , pullTemplate
    )

import File exposing (File)
import Json.Decode as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtDelete, jwtGet, jwtPostEmpty, jwtPostFile)
import Shared.Data.Template as Template exposing (Template)
import Shared.Data.TemplateDetail as TemplateDetail exposing (TemplateDetail)


getTemplates : AbstractAppState a -> ToMsg (List Template) msg -> Cmd msg
getTemplates =
    jwtGet "/templates" (D.list Template.decoder)


getTemplate : String -> AbstractAppState a -> ToMsg TemplateDetail msg -> Cmd msg
getTemplate templateId =
    jwtGet ("/templates/" ++ templateId) TemplateDetail.decoder


getTemplatesFor : String -> AbstractAppState a -> ToMsg (List Template) msg -> Cmd msg
getTemplatesFor pkgId =
    jwtGet ("/templates?pkgId=" ++ pkgId) (D.list Template.decoder)


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
exportTemplateUrl templateId appState =
    appState.apiUrl ++ "/templates/" ++ templateId ++ "/bundle"
