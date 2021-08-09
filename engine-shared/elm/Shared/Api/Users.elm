module Shared.Api.Users exposing
    ( deleteUser
    , getCurrentUser
    , getUser
    , getUsers
    , getUsersSuggestions
    , postUser
    , postUserPublic
    , putUser
    , putUserActivation
    , putUserPassword
    , putUserPasswordPublic
    )

import Json.Encode as E
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, httpPost, httpPut, jwtDelete, jwtGet, jwtPost, jwtPut)
import Shared.Data.Member as Member exposing (Member)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.User as User exposing (User)
import Shared.Data.UserSuggestion as UserSuggestion exposing (UserSuggestion)


type alias GetUsersFilters =
    { role : Maybe String }


getUsers : GetUsersFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination User) msg -> Cmd msg
getUsers filters qs =
    let
        extraParams =
            PaginationQueryString.filterParams [ ( "role", filters.role ) ]

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            "/users" ++ queryString
    in
    jwtGet url (Pagination.decoder "users" User.decoder)


getUsersSuggestions : PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination UserSuggestion) msg -> Cmd msg
getUsersSuggestions qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/users/suggestions" ++ queryString
    in
    jwtGet url (Pagination.decoder "users" UserSuggestion.decoder)


getUser : String -> AbstractAppState a -> ToMsg User msg -> Cmd msg
getUser uuid =
    jwtGet ("/users/" ++ uuid) User.decoder


getCurrentUser : AbstractAppState a -> ToMsg User msg -> Cmd msg
getCurrentUser =
    jwtGet "/users/current" User.decoder


postUser : E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
postUser =
    jwtPost "/users"


postUserPublic : E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
postUserPublic =
    httpPost "/users"


putUser : String -> E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putUser uuid =
    jwtPut ("/users/" ++ uuid)


putUserPassword : String -> E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putUserPassword uuid =
    jwtPut ("/users/" ++ uuid ++ "/password")


putUserPasswordPublic : String -> String -> E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putUserPasswordPublic uuid hash =
    httpPut ("/users/" ++ uuid ++ "/password?hash=" ++ hash)


putUserActivation : String -> String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putUserActivation uuid hash =
    let
        body =
            E.object [ ( "active", E.bool True ) ]
    in
    httpPut ("/users/" ++ uuid ++ "/state?hash=" ++ hash) body


deleteUser : String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteUser uuid =
    jwtDelete ("/users/" ++ uuid)
