module Shared.Api.UserGroups exposing (getUserGroupsSuggestions)

import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtGet)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.UserGroupSuggestion as UserGroupSuggestion exposing (UserGroupSuggestion)


getUserGroupsSuggestions : PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination UserGroupSuggestion) msg -> Cmd msg
getUserGroupsSuggestions qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/user-groups/suggestions" ++ queryString
    in
    jwtGet url (Pagination.decoder "userGroups" UserGroupSuggestion.decoder)
