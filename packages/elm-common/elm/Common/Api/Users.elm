module Common.Api.Users exposing
    ( deleteUser
    , deleteUserIdentity
    , getCurrentUserIdentities
    , getCurrentUserLocale
    , getCurrentUserPluginSettings
    , getCurrentUserSubmissionProps
    , getUser
    , getUsers
    , getUsersSuggestions
    , getUsersSuggestionsWithOptions
    , postConsents
    , postFromExternal
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

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.SubmissionProps as SubmissionProps exposing (SubmissionProps)
import Common.Api.Models.TokenResponse as TokenResponse exposing (TokenResponse)
import Common.Api.Models.User as User exposing (User)
import Common.Api.Models.UserFromExternal as UserFromExternal exposing (UserFromExternal)
import Common.Api.Models.UserIdentity as UserIdentity exposing (UserIdentity)
import Common.Api.Models.UserLocale as UserLocale exposing (UserLocale)
import Common.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Data.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Json.Decode as D
import Json.Encode as E
import Json.Encode.Extra as E
import Uuid exposing (Uuid)


getUsers : Context ctx -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination User) msg -> Cmd msg
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
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "users" User.decoder)


getUsersSuggestions : Context ctx -> PaginationQueryString -> ToMsg (Pagination UserSuggestion) msg -> Cmd msg
getUsersSuggestions appState qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/users/suggestions" ++ queryString
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "users" UserSuggestion.decoder)


getUsersSuggestionsWithOptions : Context ctx -> PaginationQueryString -> List String -> List String -> ToMsg (Pagination UserSuggestion) msg -> Cmd msg
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
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "users" UserSuggestion.decoder)


getUser : Context ctx -> UuidOrCurrent -> ToMsg User msg -> Cmd msg
getUser appState uuidOrCurrent =
    Request.get (Context.toServerInfo appState) ("/users/" ++ UuidOrCurrent.toString uuidOrCurrent) User.decoder


getCurrentUserSubmissionProps : Context ctx -> ToMsg (List SubmissionProps) msg -> Cmd msg
getCurrentUserSubmissionProps appState =
    Request.get (Context.toServerInfo appState) "/users/current/submission-props" (D.list SubmissionProps.decoder)


getCurrentUserLocale : Context ctx -> ToMsg UserLocale msg -> Cmd msg
getCurrentUserLocale appState =
    Request.get (Context.toServerInfo appState) "/users/current/locale" UserLocale.decoder


putCurrentUserLocale : Context ctx -> UserLocale -> ToMsg () msg -> Cmd msg
putCurrentUserLocale appState userLocale =
    let
        body =
            UserLocale.encode userLocale
    in
    Request.putWhatever (Context.toServerInfo appState) "/users/current/locale" body


postUser : Context ctx -> E.Value -> ToMsg () msg -> Cmd msg
postUser appState body =
    Request.postWhatever (Context.toServerInfo appState) "/users" body


postUserPublic : Context ctx -> E.Value -> ToMsg () msg -> Cmd msg
postUserPublic appState body =
    Request.postWhatever (Context.toServerInfo appState) "/users" body


putUser : Context ctx -> UuidOrCurrent -> E.Value -> ToMsg User msg -> Cmd msg
putUser appState uuidOrCurrent body =
    Request.put (Context.toServerInfo appState) ("/users/" ++ UuidOrCurrent.toString uuidOrCurrent) User.decoder body


putUserPassword : Context ctx -> UuidOrCurrent -> E.Value -> ToMsg () msg -> Cmd msg
putUserPassword appState uuidOrCurrent body =
    Request.putWhatever (Context.toServerInfo appState) ("/users/" ++ UuidOrCurrent.toString uuidOrCurrent ++ "/password") body


putUserPasswordPublic : Context ctx -> String -> String -> E.Value -> ToMsg () msg -> Cmd msg
putUserPasswordPublic appState uuid hash body =
    Request.putWhatever (Context.toServerInfo appState) ("/users/" ++ uuid ++ "/password?hash=" ++ hash) body


putUserActivation : Context ctx -> String -> String -> ToMsg () msg -> Cmd msg
putUserActivation appState uuid hash =
    let
        body =
            E.object [ ( "active", E.bool True ) ]
    in
    Request.putWhatever (Context.toServerInfo appState) ("/users/" ++ uuid ++ "/state?hash=" ++ hash) body


putCurrentUserSubmissionProps : Context ctx -> E.Value -> ToMsg () msg -> Cmd msg
putCurrentUserSubmissionProps appState body =
    Request.putWhatever (Context.toServerInfo appState) "/users/current/submission-props" body


deleteUser : Context ctx -> String -> ToMsg () msg -> Cmd msg
deleteUser appState uuid =
    Request.delete (Context.toServerInfo appState) ("/users/" ++ uuid)


postConsents : Context ctx -> String -> Maybe String -> ToMsg TokenResponse msg -> Cmd msg
postConsents appState hash mbSessionState =
    let
        body =
            E.object
                [ ( "hash", E.string hash )
                , ( "sessionState", E.maybe E.string mbSessionState )
                ]
    in
    Request.post (Context.toServerInfo appState) "/users/consents" TokenResponse.decoder body


postFromExternal : Context ctx -> UserFromExternal -> ToMsg TokenResponse msg -> Cmd msg
postFromExternal appState userFromExternal =
    Request.post (Context.toServerInfo appState) "/users/from-external" TokenResponse.decoder (UserFromExternal.encode userFromExternal)


putLastSeenNewsId : Context ctx -> String -> ToMsg () msg -> Cmd msg
putLastSeenNewsId appState lastSeenNewsId =
    Request.putEmpty (Context.toServerInfo appState) ("/users/current/news/" ++ lastSeenNewsId)


getCurrentUserPluginSettings : Context ctx -> Uuid -> ToMsg String msg -> Cmd msg
getCurrentUserPluginSettings appState pluginUuid =
    Request.getString (Context.toServerInfo appState) ("/users/current/plugin-settings/" ++ Uuid.toString pluginUuid)


putCurrentPluginSettings : Context ctx -> Uuid -> String -> ToMsg () msg -> Cmd msg
putCurrentPluginSettings appState pluginUuid =
    Request.putString (Context.toServerInfo appState) ("/users/current/plugin-settings/" ++ Uuid.toString pluginUuid) "application/json"


getCurrentUserIdentities : Context ctx -> ToMsg (List UserIdentity) msg -> Cmd msg
getCurrentUserIdentities appState =
    Request.get (Context.toServerInfo appState) "/users/current/identities" (D.list UserIdentity.decoder)


deleteUserIdentity : Context ctx -> Uuid -> ToMsg () msg -> Cmd msg
deleteUserIdentity appState identityUuid =
    Request.delete (Context.toServerInfo appState) ("/users/current/identities/" ++ Uuid.toString identityUuid)
