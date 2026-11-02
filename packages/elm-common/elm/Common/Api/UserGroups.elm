module Common.Api.UserGroups exposing
    ( deleteUserGroup
    , getUserGroup
    , getUserGroups
    , getUserGroupsSuggestions
    , getUserGroupsSuggestionsWithOptions
    , postUserGroup
    , putUserGroup
    , putUserGroupMemberships
    )

{-| The user groups of a workspace. A list, the suggestions and a
create go through the client's scope (`Common.Api.Params`); one group is
asked for by its uuid alone, since it carries its workspace.
-}

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.UserGroup as UserGroup exposing (UserGroup)
import Common.Api.Models.UserGroupDetail as UserGroupDetail exposing (UserGroupDetail)
import Common.Api.Models.UserGroupMember as UserGroupMember exposing (UserGroupMember)
import Common.Api.Models.UserGroupSuggestion as UserGroupSuggestion exposing (UserGroupSuggestion)
import Common.Api.Params as Params
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Json.Decode as D
import Json.Encode as E
import Uuid exposing (Uuid)


getUserGroupsSuggestions : Context ctx -> PaginationQueryString -> ToMsg (Pagination UserGroupSuggestion) msg -> Cmd msg
getUserGroupsSuggestions appState qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            Params.apply appState "GET" ("/user-groups/suggestions" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "userGroups" UserGroupSuggestion.decoder)


getUserGroupsSuggestionsWithOptions : Context ctx -> PaginationQueryString -> List String -> List String -> ToMsg (Pagination UserGroupSuggestion) msg -> Cmd msg
getUserGroupsSuggestionsWithOptions appState qs select exclude =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "select", String.join "," select )
                , ( "exclude", String.join "," exclude )
                ]
                qs

        url =
            Params.apply appState "GET" ("/user-groups/suggestions" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "userGroups" UserGroupSuggestion.decoder)


getUserGroups : Context ctx -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination UserGroup) msg -> Cmd msg
getUserGroups appState filters qs =
    let
        extraParams =
            PaginationQueryString.filterParams
                [ ( "member", PaginationQueryFilters.getValue "member" filters ) ]

        url =
            Params.apply appState "GET" ("/user-groups" ++ PaginationQueryString.toApiUrlWith extraParams qs)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "userGroups" UserGroup.decoder)


getUserGroup : Context ctx -> Uuid -> ToMsg UserGroupDetail msg -> Cmd msg
getUserGroup appState uuid =
    Request.get (Context.toServerInfo appState) (userGroupUrl uuid) UserGroupDetail.decoder


{-| Answers the new group's uuid.
-}
postUserGroup : Context ctx -> E.Value -> ToMsg Uuid msg -> Cmd msg
postUserGroup appState body =
    Request.post (Context.toServerInfo appState)
        (Params.apply appState "POST" "/user-groups")
        (D.field "uuid" Uuid.decoder)
        body


putUserGroup : Context ctx -> Uuid -> E.Value -> ToMsg () msg -> Cmd msg
putUserGroup appState uuid body =
    Request.putWhatever (Context.toServerInfo appState) (userGroupUrl uuid) body


deleteUserGroup : Context ctx -> Uuid -> ToMsg () msg -> Cmd msg
deleteUserGroup appState uuid =
    Request.delete (Context.toServerInfo appState) (userGroupUrl uuid)


putUserGroupMemberships : Context ctx -> Uuid -> List UserGroupMember -> ToMsg () msg -> Cmd msg
putUserGroupMemberships appState uuid members =
    Request.putWhatever (Context.toServerInfo appState)
        (userGroupUrl uuid ++ "/memberships")
        (E.list UserGroupMember.encode members)


userGroupUrl : Uuid -> String
userGroupUrl uuid =
    "/user-groups/" ++ Uuid.toString uuid
