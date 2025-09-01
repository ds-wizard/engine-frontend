module Wizard.Api.Documents exposing
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
import Shared.Api.Models.UrlResponse as UrlResponse exposing (UrlResponse)
import Shared.Api.Request as Request exposing (ToMsg)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Api.Models.Document as Document exposing (Document)
import Wizard.Api.Models.Submission as Submission exposing (Submission)
import Wizard.Api.Models.SubmissionService as SubmissionService exposing (SubmissionService)
import Wizard.Data.AppState as AppState exposing (AppState)


getDocuments : AppState -> Maybe Uuid -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination Document) msg -> Cmd msg
getDocuments appState questionnaireUuid _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "questionnaireUuid", Maybe.unwrap "" Uuid.toString questionnaireUuid ) ]
                qs

        url =
            "/documents" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "documents" Document.decoder)


postDocument : AppState -> E.Value -> ToMsg Document msg -> Cmd msg
postDocument appState body =
    Request.post (AppState.toServerInfo appState) "/documents" Document.decoder body


deleteDocument : AppState -> String -> ToMsg () msg -> Cmd msg
deleteDocument appState uuid =
    Request.delete (AppState.toServerInfo appState) ("/documents/" ++ uuid)


getSubmissionServices : AppState -> String -> ToMsg (List SubmissionService) msg -> Cmd msg
getSubmissionServices appState documentId =
    Request.get (AppState.toServerInfo appState) ("/documents/" ++ documentId ++ "/available-submission-services") (D.list SubmissionService.decoder)


getDocumentUrl : AppState -> Uuid -> ToMsg UrlResponse msg -> Cmd msg
getDocumentUrl appState uuid =
    Request.get (AppState.toServerInfo appState) ("/documents/" ++ Uuid.toString uuid ++ "/download") UrlResponse.decoder


downloadDocumentUrl : AppState -> Uuid -> String
downloadDocumentUrl appState uuid =
    appState.apiUrl ++ "/documents/" ++ Uuid.toString uuid ++ "/download"


postSubmission : AppState -> String -> String -> ToMsg Submission msg -> Cmd msg
postSubmission appState serviceId documentUuid =
    let
        body =
            E.object
                [ ( "serviceId", E.string serviceId ) ]
    in
    Request.post (AppState.toServerInfo appState) ("/documents/" ++ documentUuid ++ "/submissions") Submission.decoder body
