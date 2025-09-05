module Wizard.Api.DocumentTemplateDrafts exposing
    ( deleteAsset
    , deleteDraft
    , deleteFile
    , deleteFolder
    , getAsset
    , getAssets
    , getDraft
    , getDrafts
    , getFileContent
    , getFiles
    , getPreview
    , moveFolder
    , postDraft
    , postFile
    , putAsset
    , putDraft
    , putFile
    , putFileContent
    , putPreviewSettings
    , uploadAsset
    )

import Common.Api.Models.UrlResponse as UrlResponse exposing (UrlResponse)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.Pagination as Pagination exposing (Pagination)
import Common.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import File exposing (File)
import Http
import Json.Decode as D
import Json.Encode as E
import Uuid exposing (Uuid)
import Wizard.Api.Models.CreatedEntityWithId as CreatedEntityWithId exposing (CreatedEntityWithId)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateAsset as DocumentTemplateAsset exposing (DocumentTemplateAsset)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateFile as DocumentTemplateFile exposing (DocumentTemplateFile)
import Wizard.Api.Models.DocumentTemplateDraft as DocumentTemplateDraft exposing (DocumentTemplateDraft)
import Wizard.Api.Models.DocumentTemplateDraft.DocumentTemplateDraftPreviewSettings as DocumentTemplateDraftPreviewSettings exposing (DocumentTemplateDraftPreviewSettings)
import Wizard.Api.Models.DocumentTemplateDraftDetail as DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Wizard.Data.AppState as AppState exposing (AppState)


getDrafts : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination DocumentTemplateDraft) msg -> Cmd msg
getDrafts appState _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/document-template-drafts" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "documentTemplateDrafts" DocumentTemplateDraft.decoder)


getDraft : AppState -> String -> ToMsg DocumentTemplateDraftDetail msg -> Cmd msg
getDraft appState templateId =
    Request.get (AppState.toServerInfo appState) ("/document-template-drafts/" ++ templateId) DocumentTemplateDraftDetail.decoder


postDraft : AppState -> E.Value -> ToMsg CreatedEntityWithId msg -> Cmd msg
postDraft appState body =
    Request.post (AppState.toServerInfo appState) "/document-template-drafts" CreatedEntityWithId.decoder body


putDraft : AppState -> String -> E.Value -> ToMsg DocumentTemplateDraftDetail msg -> Cmd msg
putDraft appState templateId body =
    Request.put (AppState.toServerInfo appState) ("/document-template-drafts/" ++ templateId) DocumentTemplateDraftDetail.decoder body


deleteDraft : AppState -> String -> ToMsg () msg -> Cmd msg
deleteDraft appState templateId =
    Request.delete (AppState.toServerInfo appState) ("/document-template-drafts/" ++ templateId)


getFiles : AppState -> String -> ToMsg (List DocumentTemplateFile) msg -> Cmd msg
getFiles appState templateId =
    Request.get (AppState.toServerInfo appState) ("/document-template-drafts/" ++ templateId ++ "/files") (D.list DocumentTemplateFile.decoder)


getFileContent : AppState -> String -> Uuid -> ToMsg String msg -> Cmd msg
getFileContent appState templateId fileUuid =
    Request.getString (AppState.toServerInfo appState) ("/document-template-drafts/" ++ templateId ++ "/files/" ++ Uuid.toString fileUuid ++ "/content")


postFile : AppState -> String -> DocumentTemplateFile -> String -> ToMsg DocumentTemplateFile msg -> Cmd msg
postFile appState templateId file fileContent =
    let
        body =
            DocumentTemplateFile.encode file fileContent
    in
    Request.post (AppState.toServerInfo appState) ("/document-template-drafts/" ++ templateId ++ "/files") DocumentTemplateFile.decoder body


putFile : AppState -> String -> DocumentTemplateFile -> String -> ToMsg () msg -> Cmd msg
putFile appState templateId file fileContent =
    let
        body =
            DocumentTemplateFile.encode file fileContent
    in
    Request.putWhatever (AppState.toServerInfo appState) ("/document-template-drafts/" ++ templateId ++ "/files/" ++ Uuid.toString file.uuid) body


putFileContent : AppState -> String -> Uuid -> String -> ToMsg () msg -> Cmd msg
putFileContent appState templateId fileUuid fileContent =
    Request.putString (AppState.toServerInfo appState) ("/document-template-drafts/" ++ templateId ++ "/files/" ++ Uuid.toString fileUuid ++ "/content") "text/plain;charset=utf-8" fileContent


deleteFile : AppState -> String -> Uuid -> ToMsg () msg -> Cmd msg
deleteFile appState templateId fileUuid =
    Request.delete (AppState.toServerInfo appState) ("/document-template-drafts/" ++ templateId ++ "/files/" ++ Uuid.toString fileUuid)


getAssets : AppState -> String -> ToMsg (List DocumentTemplateAsset) msg -> Cmd msg
getAssets appState templateId =
    Request.get (AppState.toServerInfo appState) ("/document-template-drafts/" ++ templateId ++ "/assets") (D.list DocumentTemplateAsset.decoder)


getAsset : AppState -> String -> Uuid -> ToMsg DocumentTemplateAsset msg -> Cmd msg
getAsset appState templateId assetUuid =
    Request.get (AppState.toServerInfo appState) ("/document-template-drafts/" ++ templateId ++ "/assets/" ++ Uuid.toString assetUuid) DocumentTemplateAsset.decoder


putAsset : AppState -> String -> DocumentTemplateAsset -> ToMsg () msg -> Cmd msg
putAsset appState templateId asset =
    Request.putWhatever (AppState.toServerInfo appState) ("/document-template-drafts/" ++ templateId ++ "/assets/" ++ Uuid.toString asset.uuid) (DocumentTemplateAsset.encode asset)


deleteAsset : AppState -> String -> Uuid -> ToMsg () msg -> Cmd msg
deleteAsset appState templateId fileUuid =
    Request.delete (AppState.toServerInfo appState) ("/document-template-drafts/" ++ templateId ++ "/assets/" ++ Uuid.toString fileUuid)


putPreviewSettings : AppState -> String -> DocumentTemplateDraftPreviewSettings -> ToMsg DocumentTemplateDraftPreviewSettings msg -> Cmd msg
putPreviewSettings appState templateId previewSettings =
    Request.put
        (AppState.toServerInfo appState)
        ("/document-template-drafts/" ++ templateId ++ "/documents/preview/settings")
        DocumentTemplateDraftPreviewSettings.decoder
        (DocumentTemplateDraftPreviewSettings.encode previewSettings)


getPreview : AppState -> String -> ToMsg ( Http.Metadata, Maybe UrlResponse ) msg -> Cmd msg
getPreview appState templateId toMsg =
    Http.request
        { method = "GET"
        , headers = Request.authorizationHeaders (AppState.toServerInfo appState)
        , url = appState.apiUrl ++ "/document-template-drafts/" ++ templateId ++ "/documents/preview"
        , body = Http.emptyBody
        , expect = Request.expectMetadataAndJson toMsg UrlResponse.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


uploadAsset : AppState -> String -> String -> File -> ToMsg DocumentTemplateAsset msg -> Cmd msg
uploadAsset appState templateId fileName file =
    Request.postFileWithData
        (AppState.toServerInfo appState)
        ("/document-template-drafts/" ++ templateId ++ "/assets")
        file
        [ Http.stringPart "fileName" fileName ]
        DocumentTemplateAsset.decoder


moveFolder : AppState -> String -> String -> String -> ToMsg () msg -> Cmd msg
moveFolder appState templateId currentPath newPath =
    let
        body =
            E.object
                [ ( "current", E.string currentPath )
                , ( "new", E.string newPath )
                ]
    in
    Request.postWhatever (AppState.toServerInfo appState) ("/document-template-drafts/" ++ templateId ++ "/folders/move") body


deleteFolder : AppState -> String -> String -> ToMsg () msg -> Cmd msg
deleteFolder appState templateId path =
    let
        body =
            E.object
                [ ( "path", E.string path ) ]
    in
    Request.postWhatever (AppState.toServerInfo appState) ("/document-template-drafts/" ++ templateId ++ "/folders/delete") body
