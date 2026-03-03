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

import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.UrlResponse as UrlResponse exposing (UrlResponse)
import Common.Api.Models.UuidResponse as UuidResponse exposing (UuidResponse)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import File exposing (File)
import Http
import Json.Decode as D
import Json.Encode as E
import Uuid exposing (Uuid)
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


getDraft : AppState -> Uuid -> ToMsg DocumentTemplateDraftDetail msg -> Cmd msg
getDraft appState tempalteUuid =
    Request.get (AppState.toServerInfo appState) ("/document-template-drafts/" ++ Uuid.toString tempalteUuid) DocumentTemplateDraftDetail.decoder


postDraft : AppState -> E.Value -> ToMsg UuidResponse msg -> Cmd msg
postDraft appState body =
    Request.post (AppState.toServerInfo appState) "/document-template-drafts" UuidResponse.decoder body


putDraft : AppState -> Uuid -> E.Value -> ToMsg DocumentTemplateDraftDetail msg -> Cmd msg
putDraft appState templateUuid body =
    Request.put (AppState.toServerInfo appState) ("/document-template-drafts/" ++ Uuid.toString templateUuid) DocumentTemplateDraftDetail.decoder body


deleteDraft : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteDraft appState templateUuid =
    Request.delete (AppState.toServerInfo appState) ("/document-template-drafts/" ++ Uuid.toString templateUuid)


getFiles : AppState -> Uuid -> ToMsg (List DocumentTemplateFile) msg -> Cmd msg
getFiles appState templateUuid =
    Request.get (AppState.toServerInfo appState) ("/document-template-drafts/" ++ Uuid.toString templateUuid ++ "/files") (D.list DocumentTemplateFile.decoder)


getFileContent : AppState -> Uuid -> Uuid -> ToMsg String msg -> Cmd msg
getFileContent appState templateUuid fileUuid =
    Request.getString (AppState.toServerInfo appState) ("/document-template-drafts/" ++ Uuid.toString templateUuid ++ "/files/" ++ Uuid.toString fileUuid ++ "/content")


postFile : AppState -> Uuid -> DocumentTemplateFile -> String -> ToMsg DocumentTemplateFile msg -> Cmd msg
postFile appState templateUuid file fileContent =
    let
        body =
            DocumentTemplateFile.encode file fileContent
    in
    Request.post (AppState.toServerInfo appState) ("/document-template-drafts/" ++ Uuid.toString templateUuid ++ "/files") DocumentTemplateFile.decoder body


putFile : AppState -> Uuid -> DocumentTemplateFile -> String -> ToMsg () msg -> Cmd msg
putFile appState templateUuid file fileContent =
    let
        body =
            DocumentTemplateFile.encode file fileContent
    in
    Request.putWhatever (AppState.toServerInfo appState) ("/document-template-drafts/" ++ Uuid.toString templateUuid ++ "/files/" ++ Uuid.toString file.uuid) body


putFileContent : AppState -> Uuid -> Uuid -> String -> ToMsg () msg -> Cmd msg
putFileContent appState templateUuid fileUuid fileContent =
    Request.putString (AppState.toServerInfo appState) ("/document-template-drafts/" ++ Uuid.toString templateUuid ++ "/files/" ++ Uuid.toString fileUuid ++ "/content") "text/plain;charset=utf-8" fileContent


deleteFile : AppState -> Uuid -> Uuid -> ToMsg () msg -> Cmd msg
deleteFile appState templateUuid fileUuid =
    Request.delete (AppState.toServerInfo appState) ("/document-template-drafts/" ++ Uuid.toString templateUuid ++ "/files/" ++ Uuid.toString fileUuid)


getAssets : AppState -> Uuid -> ToMsg (List DocumentTemplateAsset) msg -> Cmd msg
getAssets appState templateUuid =
    Request.get (AppState.toServerInfo appState) ("/document-template-drafts/" ++ Uuid.toString templateUuid ++ "/assets") (D.list DocumentTemplateAsset.decoder)


getAsset : AppState -> Uuid -> Uuid -> ToMsg DocumentTemplateAsset msg -> Cmd msg
getAsset appState templateUuid assetUuid =
    Request.get (AppState.toServerInfo appState) ("/document-template-drafts/" ++ Uuid.toString templateUuid ++ "/assets/" ++ Uuid.toString assetUuid) DocumentTemplateAsset.decoder


putAsset : AppState -> Uuid -> DocumentTemplateAsset -> ToMsg () msg -> Cmd msg
putAsset appState templateUuid asset =
    Request.putWhatever (AppState.toServerInfo appState) ("/document-template-drafts/" ++ Uuid.toString templateUuid ++ "/assets/" ++ Uuid.toString asset.uuid) (DocumentTemplateAsset.encode asset)


deleteAsset : AppState -> Uuid -> Uuid -> ToMsg () msg -> Cmd msg
deleteAsset appState templateUuid fileUuid =
    Request.delete (AppState.toServerInfo appState) ("/document-template-drafts/" ++ Uuid.toString templateUuid ++ "/assets/" ++ Uuid.toString fileUuid)


putPreviewSettings : AppState -> Uuid -> DocumentTemplateDraftPreviewSettings -> ToMsg DocumentTemplateDraftPreviewSettings msg -> Cmd msg
putPreviewSettings appState templateUuid previewSettings =
    Request.put
        (AppState.toServerInfo appState)
        ("/document-template-drafts/" ++ Uuid.toString templateUuid ++ "/documents/preview/settings")
        DocumentTemplateDraftPreviewSettings.decoder
        (DocumentTemplateDraftPreviewSettings.encode previewSettings)


getPreview : AppState -> Uuid -> ToMsg ( Http.Metadata, Maybe UrlResponse ) msg -> Cmd msg
getPreview appState templateUuid toMsg =
    Http.request
        { method = "GET"
        , headers = Request.authorizationHeaders (AppState.toServerInfo appState)
        , url = appState.apiUrl ++ "/document-template-drafts/" ++ Uuid.toString templateUuid ++ "/documents/preview"
        , body = Http.emptyBody
        , expect = Request.expectMetadataAndJson toMsg UrlResponse.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


uploadAsset : AppState -> Uuid -> String -> File -> ToMsg DocumentTemplateAsset msg -> Cmd msg
uploadAsset appState templateUuid fileName file =
    Request.postFileWithData
        (AppState.toServerInfo appState)
        ("/document-template-drafts/" ++ Uuid.toString templateUuid ++ "/assets")
        file
        [ Http.stringPart "fileName" fileName ]
        DocumentTemplateAsset.decoder


moveFolder : AppState -> Uuid -> String -> String -> ToMsg () msg -> Cmd msg
moveFolder appState templateUuid currentPath newPath =
    let
        body =
            E.object
                [ ( "current", E.string currentPath )
                , ( "new", E.string newPath )
                ]
    in
    Request.postWhatever (AppState.toServerInfo appState) ("/document-template-drafts/" ++ Uuid.toString templateUuid ++ "/folders/move") body


deleteFolder : AppState -> Uuid -> String -> ToMsg () msg -> Cmd msg
deleteFolder appState templateUuid path =
    let
        body =
            E.object
                [ ( "path", E.string path ) ]
    in
    Request.postWhatever (AppState.toServerInfo appState) ("/document-template-drafts/" ++ Uuid.toString templateUuid ++ "/folders/delete") body
