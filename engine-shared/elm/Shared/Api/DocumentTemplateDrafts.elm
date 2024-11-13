module Shared.Api.DocumentTemplateDrafts exposing
    ( deleteAsset
    , deleteDraft
    , deleteFile
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

import File exposing (File)
import Http
import Json.Decode as D
import Json.Encode as E
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, authorizationHeaders, expectMetadataAndJson, jwtDelete, jwtFetch, jwtFetchFileWithData, jwtFetchPut, jwtGet, jwtGetString, jwtPost, jwtPut, jwtPutString)
import Shared.Data.DocumentTemplate.DocumentTemplateAsset as DocumentTemplateAsset exposing (DocumentTemplateAsset)
import Shared.Data.DocumentTemplate.DocumentTemplateFile as DocumentTemplateFile exposing (DocumentTemplateFile)
import Shared.Data.DocumentTemplateDraft as DocumentTemplateDraft exposing (DocumentTemplateDraft)
import Shared.Data.DocumentTemplateDraft.DocumentTemplateDraftPreviewSettings as DocumentTemplateDraftPreviewSettings exposing (DocumentTemplateDraftPreviewSettings)
import Shared.Data.DocumentTemplateDraftDetail as DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.UrlResponse as UrlResponse exposing (UrlResponse)
import Uuid exposing (Uuid)


getDrafts : PaginationQueryFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination DocumentTemplateDraft) msg -> Cmd msg
getDrafts _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/document-template-drafts" ++ queryString
    in
    jwtGet url (Pagination.decoder "documentTemplateDrafts" DocumentTemplateDraft.decoder)


getDraft : String -> AbstractAppState a -> ToMsg DocumentTemplateDraftDetail msg -> Cmd msg
getDraft templateId =
    jwtGet ("/document-template-drafts/" ++ templateId) DocumentTemplateDraftDetail.decoder


postDraft : E.Value -> AbstractAppState a -> ToMsg DocumentTemplateDraftDetail msg -> Cmd msg
postDraft =
    jwtFetch "/document-template-drafts" DocumentTemplateDraftDetail.decoder


putDraft : String -> E.Value -> AbstractAppState a -> ToMsg DocumentTemplateDraftDetail msg -> Cmd msg
putDraft templateId =
    jwtFetchPut ("/document-template-drafts/" ++ templateId) DocumentTemplateDraftDetail.decoder


deleteDraft : String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteDraft templateId =
    jwtDelete ("/document-template-drafts/" ++ templateId)


getFiles : String -> AbstractAppState a -> ToMsg (List DocumentTemplateFile) msg -> Cmd msg
getFiles templateId =
    jwtGet ("/document-template-drafts/" ++ templateId ++ "/files") (D.list DocumentTemplateFile.decoder)


getFileContent : String -> Uuid -> AbstractAppState a -> ToMsg String msg -> Cmd msg
getFileContent templateId fileUuid =
    jwtGetString ("/document-template-drafts/" ++ templateId ++ "/files/" ++ Uuid.toString fileUuid ++ "/content")


postFile : String -> DocumentTemplateFile -> String -> AbstractAppState a -> ToMsg DocumentTemplateFile msg -> Cmd msg
postFile templateId file fileContent =
    jwtFetch ("/document-template-drafts/" ++ templateId ++ "/files") DocumentTemplateFile.decoder (DocumentTemplateFile.encode file fileContent)


putFile : String -> DocumentTemplateFile -> String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putFile templateId file fileContent =
    jwtPut ("/document-template-drafts/" ++ templateId ++ "/files/" ++ Uuid.toString file.uuid) (DocumentTemplateFile.encode file fileContent)


putFileContent : String -> Uuid -> String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putFileContent templateId fileUuid fileContent =
    jwtPutString ("/document-template-drafts/" ++ templateId ++ "/files/" ++ Uuid.toString fileUuid ++ "/content") "text/plain;charset=utf-8" fileContent


deleteFile : String -> Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteFile templateId fileUuid =
    jwtDelete ("/document-template-drafts/" ++ templateId ++ "/files/" ++ Uuid.toString fileUuid)


getAssets : String -> AbstractAppState a -> ToMsg (List DocumentTemplateAsset) msg -> Cmd msg
getAssets templateId =
    jwtGet ("/document-template-drafts/" ++ templateId ++ "/assets") (D.list DocumentTemplateAsset.decoder)


getAsset : String -> Uuid -> AbstractAppState a -> ToMsg DocumentTemplateAsset msg -> Cmd msg
getAsset templateId assetUuid =
    jwtGet ("/document-template-drafts/" ++ templateId ++ "/assets/" ++ Uuid.toString assetUuid) DocumentTemplateAsset.decoder


putAsset : String -> DocumentTemplateAsset -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putAsset templateId asset =
    jwtPut ("/document-template-drafts/" ++ templateId ++ "/assets/" ++ Uuid.toString asset.uuid) (DocumentTemplateAsset.encode asset)


deleteAsset : String -> Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteAsset templateId fileUuid =
    jwtDelete ("/document-template-drafts/" ++ templateId ++ "/assets/" ++ Uuid.toString fileUuid)


putPreviewSettings : String -> DocumentTemplateDraftPreviewSettings -> AbstractAppState a -> ToMsg DocumentTemplateDraftPreviewSettings msg -> Cmd msg
putPreviewSettings templateId previewSettings =
    jwtFetchPut ("/document-template-drafts/" ++ templateId ++ "/documents/preview/settings")
        DocumentTemplateDraftPreviewSettings.decoder
        (DocumentTemplateDraftPreviewSettings.encode previewSettings)


getPreview : String -> AbstractAppState a -> ToMsg ( Http.Metadata, Maybe UrlResponse ) msg -> Cmd msg
getPreview templateId appState toMsg =
    Http.request
        { method = "GET"
        , headers = authorizationHeaders appState
        , url = appState.apiUrl ++ "/document-template-drafts/" ++ templateId ++ "/documents/preview"
        , body = Http.emptyBody
        , expect = expectMetadataAndJson toMsg UrlResponse.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


uploadAsset : String -> String -> File -> AbstractAppState a -> ToMsg DocumentTemplateAsset msg -> Cmd msg
uploadAsset templateId fileName =
    jwtFetchFileWithData ("/document-template-drafts/" ++ templateId ++ "/assets")
        [ Http.stringPart "fileName" fileName ]
        DocumentTemplateAsset.decoder


moveFolder : String -> String -> String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
moveFolder templateId currentPath newPath =
    let
        body =
            E.object
                [ ( "current", E.string currentPath )
                , ( "new", E.string newPath )
                ]
    in
    jwtPost ("/document-template-drafts/" ++ templateId ++ "/folders/move") body
