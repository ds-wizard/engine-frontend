module Shared.Api.Documents exposing
    ( deleteDocument
    , downloadDocumentUrl
    , getDocumentUrl
    , getDocuments
    , getSubmissionServices
    , postDocument
    , postSubmission
    )

import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as Maybe
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtDelete, jwtFetch, jwtGet)
import Shared.Data.Document as Document exposing (Document)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.Submission as Submission exposing (Submission)
import Shared.Data.SubmissionService as SubmissionService exposing (SubmissionService)
import Shared.Data.UrlResponse as UrlResponse exposing (UrlResponse)
import Uuid exposing (Uuid)


getDocuments : Maybe Uuid -> PaginationQueryFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination Document) msg -> Cmd msg
getDocuments questionnaireUuid _ qs =
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


getDocumentUrl : Uuid -> AbstractAppState a -> ToMsg UrlResponse msg -> Cmd msg
getDocumentUrl uuid =
    jwtGet ("/documents/" ++ Uuid.toString uuid ++ "/download") UrlResponse.decoder


downloadDocumentUrl : Uuid -> AbstractAppState a -> String
downloadDocumentUrl uuid appState =
    appState.apiUrl ++ "/documents/" ++ Uuid.toString uuid ++ "/download"


postSubmission : String -> String -> AbstractAppState a -> ToMsg Submission msg -> Cmd msg
postSubmission serviceId documentUuid =
    let
        body =
            E.object
                [ ( "serviceId", E.string serviceId )
                ]
    in
    jwtFetch ("/documents/" ++ documentUuid ++ "/submissions") Submission.decoder body
