module Common.Api.Workspaces exposing
    ( getMemberSuggestions
    , getMembers
    )

{-| The members of a workspace, the users who see what is in it: what is given
to somebody inside a workspace — a place in one of its user groups — is given
to one of them.
-}

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Common.Api.Models.WorkspaceMember as WorkspaceMember exposing (WorkspaceMember)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Json.Decode as D
import Uuid exposing (Uuid)


getMembers : Context ctx -> Uuid -> PaginationQueryString -> ToMsg (Pagination WorkspaceMember) msg -> Cmd msg
getMembers appState uuid qs =
    Request.get (Context.toServerInfo appState)
        ("/workspaces/" ++ Uuid.toString uuid ++ "/members" ++ PaginationQueryString.toApiUrl qs)
        (Pagination.decoder "members" WorkspaceMember.decoder)


{-| The members of a workspace as users to pick from, for giving one of them
something inside it — a share of one of its projects. A member has no
affiliation.
-}
getMemberSuggestions : Context ctx -> Uuid -> PaginationQueryString -> ToMsg (Pagination UserSuggestion) msg -> Cmd msg
getMemberSuggestions appState uuid qs =
    let
        toSuggestion member =
            { uuid = member.uuid
            , firstName = member.firstName
            , lastName = member.lastName
            , gravatarHash = member.gravatarHash
            , imageUrl = member.imageUrl
            , affiliation = Nothing
            }
    in
    Request.get (Context.toServerInfo appState)
        ("/workspaces/" ++ Uuid.toString uuid ++ "/members" ++ PaginationQueryString.toApiUrl qs)
        (Pagination.decoder "members" (D.map toSuggestion WorkspaceMember.decoder))
