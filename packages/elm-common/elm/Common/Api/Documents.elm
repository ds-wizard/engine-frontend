module Common.Api.Documents exposing
    ( deleteDocument
    , downloadDocumentUrl
    , getDocumentUrl
    , getDocuments
    , getSubmissionServices
    , postDocument
    , postSubmission
    )

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.Document as Document exposing (Document)
import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.Submission as Submission exposing (Submission)
import Common.Api.Models.SubmissionService as SubmissionService exposing (SubmissionService)
import Common.Api.Models.UrlResponse as UrlResponse exposing (UrlResponse)
import Common.Api.Params as Params
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)


getDocuments : Context ctx -> Maybe Uuid -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination Document) msg -> Cmd msg
getDocuments appState projectUuid _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "projectUuid", Maybe.unwrap "" Uuid.toString projectUuid ) ]
                qs

        url =
            Params.apply appState "GET" ("/documents" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "documents" Document.decoder)


postDocument : Context ctx -> E.Value -> ToMsg Document msg -> Cmd msg
postDocument appState body =
    Request.post (Context.toServerInfo appState) "/documents" Document.decoder body


deleteDocument : Context ctx -> String -> ToMsg () msg -> Cmd msg
deleteDocument appState uuid =
    Request.delete (Context.toServerInfo appState) ("/documents/" ++ uuid)


getSubmissionServices : Context ctx -> String -> ToMsg (List SubmissionService) msg -> Cmd msg
getSubmissionServices appState documentId =
    Request.get (Context.toServerInfo appState) ("/documents/" ++ documentId ++ "/available-submission-services") (D.list SubmissionService.decoder)


getDocumentUrl : Context ctx -> Uuid -> ToMsg UrlResponse msg -> Cmd msg
getDocumentUrl appState uuid =
    Request.get (Context.toServerInfo appState) ("/documents/" ++ Uuid.toString uuid ++ "/download") UrlResponse.decoder


downloadDocumentUrl : Uuid -> String
downloadDocumentUrl uuid =
    "/documents/" ++ Uuid.toString uuid ++ "/download"


postSubmission : Context ctx -> String -> String -> ToMsg Submission msg -> Cmd msg
postSubmission appState serviceId documentUuid =
    let
        body =
            E.object
                [ ( "serviceId", E.string serviceId ) ]
    in
    Request.post (Context.toServerInfo appState) ("/documents/" ++ documentUuid ++ "/submissions") Submission.decoder body
