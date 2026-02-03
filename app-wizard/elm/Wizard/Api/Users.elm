module Wizard.Api.Users exposing
    ( deleteUser
    , getCurrentUserLocale
    , getCurrentUserPluginSettings
    , getCurrentUserSubmissionProps
    , getUser
    , getUsers
    , getUsersSuggestions
    , getUsersSuggestionsWithOptions
    , postUser
    , postUserPublic
    , putCurrentPluginSettings
    , putCurrentUserLocale
    , putCurrentUserSubmissionProps
    , putLastSeenNewsId
    , putUser
    , putUserActivation
    , putUserPassword
    , putUserPasswordPublic
    )

import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Data.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Json.Decode as D
import Json.Encode as E
import Uuid exposing (Uuid)
import Wizard.Api.Models.SubmissionProps as SubmissionProps exposing (SubmissionProps)
import Wizard.Api.Models.User as User exposing (User)
import Wizard.Api.Models.UserLocale as UserLocale exposing (UserLocale)
import Wizard.Data.AppState as AppState exposing (AppState)


getUsers : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination User) msg -> Cmd msg
getUsers appState filters qs =
    let
        extraParams =
            PaginationQueryString.filterParams
                [ ( "role", PaginationQueryFilters.getValue "role" filters ) ]

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            "/users" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "users" User.decoder)


getUsersSuggestions : AppState -> PaginationQueryString -> ToMsg (Pagination UserSuggestion) msg -> Cmd msg
getUsersSuggestions appState qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/users/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "users" UserSuggestion.decoder)


getUsersSuggestionsWithOptions : AppState -> PaginationQueryString -> List String -> List String -> ToMsg (Pagination UserSuggestion) msg -> Cmd msg
getUsersSuggestionsWithOptions appState qs select exclude =
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
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "users" UserSuggestion.decoder)


getUser : AppState -> UuidOrCurrent -> ToMsg User msg -> Cmd msg
getUser appState uuidOrCurrent =
    Request.get (AppState.toServerInfo appState) ("/users/" ++ UuidOrCurrent.toString uuidOrCurrent) User.decoder


getCurrentUserSubmissionProps : AppState -> ToMsg (List SubmissionProps) msg -> Cmd msg
getCurrentUserSubmissionProps appState =
    Request.get (AppState.toServerInfo appState) "/users/current/submission-props" (D.list SubmissionProps.decoder)


getCurrentUserLocale : AppState -> ToMsg UserLocale msg -> Cmd msg
getCurrentUserLocale appState =
    Request.get (AppState.toServerInfo appState) "/users/current/locale" UserLocale.decoder


putCurrentUserLocale : AppState -> UserLocale -> ToMsg () msg -> Cmd msg
putCurrentUserLocale appState userLocale =
    let
        body =
            UserLocale.encode userLocale
    in
    Request.putWhatever (AppState.toServerInfo appState) "/users/current/locale" body


postUser : AppState -> E.Value -> ToMsg () msg -> Cmd msg
postUser appState body =
    Request.postWhatever (AppState.toServerInfo appState) "/users" body


postUserPublic : AppState -> E.Value -> ToMsg () msg -> Cmd msg
postUserPublic appState body =
    Request.postWhatever (AppState.toServerInfo appState) "/users" body


putUser : AppState -> UuidOrCurrent -> E.Value -> ToMsg User msg -> Cmd msg
putUser appState uuidOrCurrent body =
    Request.put (AppState.toServerInfo appState) ("/users/" ++ UuidOrCurrent.toString uuidOrCurrent) User.decoder body


putUserPassword : AppState -> UuidOrCurrent -> E.Value -> ToMsg () msg -> Cmd msg
putUserPassword appState uuidOrCurrent body =
    Request.putWhatever (AppState.toServerInfo appState) ("/users/" ++ UuidOrCurrent.toString uuidOrCurrent ++ "/password") body


putUserPasswordPublic : AppState -> String -> String -> E.Value -> ToMsg () msg -> Cmd msg
putUserPasswordPublic appState uuid hash body =
    Request.putWhatever (AppState.toServerInfo appState) ("/users/" ++ uuid ++ "/password?hash=" ++ hash) body


putUserActivation : AppState -> String -> String -> ToMsg () msg -> Cmd msg
putUserActivation appState uuid hash =
    let
        body =
            E.object [ ( "active", E.bool True ) ]
    in
    Request.putWhatever (AppState.toServerInfo appState) ("/users/" ++ uuid ++ "/state?hash=" ++ hash) body


putCurrentUserSubmissionProps : AppState -> E.Value -> ToMsg () msg -> Cmd msg
putCurrentUserSubmissionProps appState body =
    Request.putWhatever (AppState.toServerInfo appState) "/users/current/submission-props" body


deleteUser : AppState -> String -> ToMsg () msg -> Cmd msg
deleteUser appState uuid =
    Request.delete (AppState.toServerInfo appState) ("/users/" ++ uuid)


putLastSeenNewsId : AppState -> String -> ToMsg () msg -> Cmd msg
putLastSeenNewsId appState lastSeenNewsId =
    Request.putEmpty (AppState.toServerInfo appState) ("/users/current/news/" ++ lastSeenNewsId)


getCurrentUserPluginSettings : AppState -> Uuid -> ToMsg String msg -> Cmd msg
getCurrentUserPluginSettings appState pluginUuid =
    Request.getString (AppState.toServerInfo appState) ("/users/current/plugin-settings/" ++ Uuid.toString pluginUuid)


putCurrentPluginSettings : AppState -> Uuid -> String -> ToMsg () msg -> Cmd msg
putCurrentPluginSettings appState pluginUuid =
    Request.putString (AppState.toServerInfo appState) ("/users/current/plugin-settings/" ++ Uuid.toString pluginUuid) "application/json"
