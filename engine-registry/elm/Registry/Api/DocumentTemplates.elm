module Registry.Api.DocumentTemplates exposing (getDocumentTemplate, getDocumentTemplates)

import Json.Decode as D
import Registry.Api.Models.DocumentTemplate as DocumentTemplate exposing (DocumentTemplate)
import Registry.Api.Models.DocumentTemplateDetail as DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Registry.Api.Requests as Requests
import Registry.Data.AppState exposing (AppState)
import Shared.Api exposing (ToMsg)


getDocumentTemplates : AppState -> ToMsg (List DocumentTemplate) msg -> Cmd msg
getDocumentTemplates appState =
    Requests.get appState "/document-templates" (D.list DocumentTemplate.decoder)


getDocumentTemplate : AppState -> String -> ToMsg DocumentTemplateDetail msg -> Cmd msg
getDocumentTemplate appState documentTemplateId =
    Requests.get appState ("/document-templates/" ++ documentTemplateId) DocumentTemplateDetail.decoder
