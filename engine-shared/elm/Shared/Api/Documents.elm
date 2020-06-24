module Shared.Api.Documents exposing (deleteDocument, downloadDocumentUrl, getDocuments, getSubmissionServices, postDocument)

import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as Maybe
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtDelete, jwtFetch, jwtGet)
import Shared.Data.Document as Document exposing (Document)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.SubmissionService as SubmissionService exposing (SubmissionService)
import Uuid exposing (Uuid)


getDocuments : Maybe Uuid -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination Document) msg -> Cmd msg
getDocuments questionnaireUuid qs =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "questionnaireUuid", Maybe.unwrap "" Uuid.toString questionnaireUuid ) ]
                qs

        url =
            "/documents" ++ queryString
    in
    jwtGet url (Pagination.decoder "documents" Document.decoder)


postDocument : E.Value -> AbstractAppState a -> ToMsg Document msg -> Cmd msg
postDocument =
    jwtFetch "/documents" Document.decoder


deleteDocument : String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteDocument uuid =
    jwtDelete ("/documents/" ++ uuid)


getSubmissionServices : String -> AbstractAppState a -> ToMsg (List SubmissionService) msg -> Cmd msg
getSubmissionServices documentId =
    jwtGet ("/documents/" ++ documentId ++ "/available-submission-services") (D.list SubmissionService.decoder)


downloadDocumentUrl : String -> AbstractAppState a -> String
downloadDocumentUrl uuid appState =
    appState.apiUrl ++ "/documents/" ++ uuid ++ "/download"
