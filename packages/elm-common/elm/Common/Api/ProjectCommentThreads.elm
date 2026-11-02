module Common.Api.ProjectCommentThreads exposing (getCommentThreads)

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.ProjectCommentThreadAssigned as ProjectCommentThreadAssigned exposing (ProjectCommentThreadAssigned)
import Common.Api.Params as Params
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)


getCommentThreads : Context ctx -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination ProjectCommentThreadAssigned) msg -> Cmd msg
getCommentThreads appState filters qs =
    let
        extraParams =
            PaginationQueryString.filterParams
                [ ( "resolved", PaginationQueryFilters.getValue "resolved" filters ) ]

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            Params.apply appState "GET" ("/project-comment-threads" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "projectCommentThreads" ProjectCommentThreadAssigned.decoder)
