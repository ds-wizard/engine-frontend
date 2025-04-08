module Shared.Api.Users exposing
    ( deleteUser
    , getCurrentUserLocale
    , getCurrentUserSubmissionProps
    , getUser
    , getUsers
    , getUsersSuggestions
    , getUsersSuggestionsWithOptions
    , postUser
    , postUserPublic
    , putCurrentUserLocale
    , putCurrentUserSubmissionProps
    , putUser
    , putUserActivation
    , putUserPassword
    , putUserPasswordPublic
    )

import Json.Decode as D
import Json.Encode as E
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, httpPost, httpPut, jwtDelete, jwtFetchPut, jwtGet, jwtPost, jwtPut)
import Shared.Common.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.SubmissionProps as SubmissionProps exposing (SubmissionProps)
import Shared.Data.User as User exposing (User)
import Shared.Data.UserLocale as UserLocale exposing (UserLocale)
import Shared.Data.UserSuggestion as UserSuggestion exposing (UserSuggestion)


getUsers : PaginationQueryFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination User) msg -> Cmd msg
getUsers filters qs =
    let
        extraParams =
            PaginationQueryString.filterParams
                [ ( "role", PaginationQueryFilters.getValue "role" filters )
                ]

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


getUsersSuggestionsWithOptions : PaginationQueryString -> List String -> List String -> AbstractAppState a -> ToMsg (Pagination UserSuggestion) msg -> Cmd msg
getUsersSuggestionsWithOptions qs select exclude =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "select", String.join "," select )
                , ( "exclude", String.join "," exclude )
                ]
                qs

        url =
            "/users/suggestions" ++ queryString
    in
    jwtGet url (Pagination.decoder "users" UserSuggestion.decoder)


getUser : UuidOrCurrent -> AbstractAppState a -> ToMsg User msg -> Cmd msg
getUser uuidOrCurrent =
    jwtGet ("/users/" ++ UuidOrCurrent.toString uuidOrCurrent) User.decoder


getCurrentUserSubmissionProps : AbstractAppState a -> ToMsg (List SubmissionProps) msg -> Cmd msg
getCurrentUserSubmissionProps =
    jwtGet "/users/current/submission-props" (D.list SubmissionProps.decoder)


getCurrentUserLocale : AbstractAppState a -> ToMsg UserLocale msg -> Cmd msg
getCurrentUserLocale =
    jwtGet "/users/current/locale" UserLocale.decoder


putCurrentUserLocale : AbstractAppState a -> UserLocale -> ToMsg () msg -> Cmd msg
putCurrentUserLocale appState userLocale =
    let
        body =
            UserLocale.encode userLocale
    in
    jwtPut "/users/current/locale" body appState


postUser : E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
postUser =
    jwtPost "/users"


postUserPublic : E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
postUserPublic =
    httpPost "/users"


putUser : UuidOrCurrent -> E.Value -> AbstractAppState a -> ToMsg User msg -> Cmd msg
putUser uuidOrCurrent =
    jwtFetchPut ("/users/" ++ UuidOrCurrent.toString uuidOrCurrent) User.decoder


putUserPassword : UuidOrCurrent -> E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putUserPassword uuidOrCurrent =
    jwtPut ("/users/" ++ UuidOrCurrent.toString uuidOrCurrent ++ "/password")


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


putCurrentUserSubmissionProps : E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putCurrentUserSubmissionProps =
    jwtPut "/users/current/submission-props"


deleteUser : String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteUser uuid =
    jwtDelete ("/users/" ++ uuid)
