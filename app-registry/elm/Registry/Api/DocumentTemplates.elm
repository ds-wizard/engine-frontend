module Registry.Api.DocumentTemplates exposing (getDocumentTemplate, getDocumentTemplates)

import Common.Api.Request as Requests exposing (ToMsg)
import Json.Decode as D
import Registry.Api.Models.DocumentTemplate as DocumentTemplate exposing (DocumentTemplate)
import Registry.Api.Models.DocumentTemplateDetail as DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Registry.Data.AppState as AppState exposing (AppState)


getDocumentTemplates : AppState -> ToMsg (List DocumentTemplate) msg -> Cmd msg
getDocumentTemplates appState =
    Requests.get (AppState.toServerInfo appState) "/document-templates" (D.list DocumentTemplate.decoder)


getDocumentTemplate : AppState -> String -> ToMsg DocumentTemplateDetail msg -> Cmd msg
getDocumentTemplate appState documentTemplateId =
    Requests.get (AppState.toServerInfo appState) ("/document-templates/" ++ documentTemplateId) DocumentTemplateDetail.decoder
