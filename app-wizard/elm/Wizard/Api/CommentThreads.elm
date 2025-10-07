module Wizard.Api.CommentThreads exposing (getCommentThreads)

import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.QuestionnaireCommentThreadAssigned as QuestionnaireCommentThreadAssigned exposing (QuestionnaireCommentThreadAssigned)
import Wizard.Data.AppState as AppState exposing (AppState)


getCommentThreads : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination QuestionnaireCommentThreadAssigned) msg -> Cmd msg
getCommentThreads appState filters qs =
    let
        extraParams =
            PaginationQueryString.filterParams
                [ ( "resolved", PaginationQueryFilters.getValue "resolved" filters ) ]

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            "/comment-threads" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "commentThreads" QuestionnaireCommentThreadAssigned.decoder)
