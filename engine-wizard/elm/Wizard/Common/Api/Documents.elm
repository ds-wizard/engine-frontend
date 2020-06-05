module Wizard.Common.Api.Documents exposing
    ( deleteDocument
    , downloadDocumentUrl
    , getDocuments
    , getDocumentsTracker
    , getSubmissionServices
    , postDocument
    )

import Json.Decode as D
import Json.Encode exposing (Value)
import Wizard.Common.Api exposing (ToMsg, jwtDelete, jwtFetch, jwtGet, jwtGetWithTracker)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Pagination.Pagination as Pagination exposing (Pagination)
import Wizard.Common.Pagination.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Wizard.Documents.Common.Document as Document exposing (Document)
import Wizard.Documents.Common.SubmissionService as SubmissionService exposing (SubmissionService)


getDocumentsTracker : String
getDocumentsTracker =
    "getDocumentsTracker"


getDocuments : Maybe String -> PaginationQueryString -> AppState -> ToMsg (Pagination Document) msg -> Cmd msg
getDocuments questionnaireUuid qs =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "questionnaireUuid", Maybe.withDefault "" questionnaireUuid ) ]
                qs

        url =
            "/documents" ++ queryString
    in
    jwtGetWithTracker getDocumentsTracker url (Pagination.decoder "documents" Document.decoder)


postDocument : Value -> AppState -> ToMsg Document msg -> Cmd msg
postDocument =
    jwtFetch "/documents" Document.decoder


deleteDocument : String -> AppState -> ToMsg () msg -> Cmd msg
deleteDocument uuid =
    jwtDelete ("/documents/" ++ uuid)


getSubmissionServices : String -> AppState -> ToMsg (List SubmissionService) msg -> Cmd msg
getSubmissionServices documentId =
    jwtGet ("/documents/" ++ documentId ++ "/available-submission-services") (D.list SubmissionService.decoder)


downloadDocumentUrl : String -> AppState -> String
downloadDocumentUrl uuid appState =
    appState.apiUrl ++ "/documents/" ++ uuid ++ "/download"
