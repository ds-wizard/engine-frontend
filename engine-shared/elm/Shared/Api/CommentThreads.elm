module Shared.Api.CommentThreads exposing (getCommentThreads)

import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtGet)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.QuestionnaireCommentThreadAssigned as QuestionnaireCommentThreadAssigned exposing (QuestionnaireCommentThreadAssigned)


getCommentThreads : PaginationQueryFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination QuestionnaireCommentThreadAssigned) msg -> Cmd msg
getCommentThreads filters qs =
    let
        extraParams =
            PaginationQueryString.filterParams
                [ ( "resolved", PaginationQueryFilters.getValue "resolved" filters )
                ]

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            "/comment-threads" ++ queryString
    in
    jwtGet url (Pagination.decoder "commentThreads" QuestionnaireCommentThreadAssigned.decoder)
