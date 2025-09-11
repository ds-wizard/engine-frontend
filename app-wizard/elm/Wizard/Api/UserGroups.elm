module Wizard.Api.UserGroups exposing (getUserGroupsSuggestions)

import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.UserGroupSuggestion as UserGroupSuggestion exposing (UserGroupSuggestion)
import Wizard.Data.AppState as AppState exposing (AppState)


getUserGroupsSuggestions : AppState -> PaginationQueryString -> ToMsg (Pagination UserGroupSuggestion) msg -> Cmd msg
getUserGroupsSuggestions appState qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/user-groups/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "userGroups" UserGroupSuggestion.decoder)
