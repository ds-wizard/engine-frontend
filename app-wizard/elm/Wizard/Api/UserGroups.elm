module Wizard.Api.UserGroups exposing
    ( getUserGroupsSuggestions
    , getUserGroupsSuggestionsWithOptions
    )

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


getUserGroupsSuggestionsWithOptions : AppState -> PaginationQueryString -> List String -> List String -> ToMsg (Pagination UserGroupSuggestion) msg -> Cmd msg
getUserGroupsSuggestionsWithOptions appState qs select exclude =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "select", String.join "," select )
                , ( "exclude", String.join "," exclude )
                ]
                qs

        url =
            "/user-groups/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "userGroups" UserGroupSuggestion.decoder)
