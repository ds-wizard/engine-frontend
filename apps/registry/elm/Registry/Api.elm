module Registry.Api exposing
    ( deleteApiKey
    , deleteUser
    , getApiKeys
    , getBuildInfo
    , getCurrentUser
    , getItem
    , getItems
    , getTranslations
    , getUsers
    , postApiKey
    , postForgottenPassword
    , postToken
    , postUser
    , putCurrentPassword
    , putCurrentUser
    , putPassword
    , putUserActivation
    , translationPoUrl
    )

{-| Every request of the registry.

What is published is read by anybody; an account, its API keys and the
accounts an administrator manages need the session's token, which
`Common.Api.Request` sends when there is one.

-}

import Common.Api.Models.BuildInfo as BuildInfo exposing (BuildInfo)
import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Request as Request exposing (ToMsg)
import Json.Decode as D
import Json.Decode.Extra as D
import Json.Encode as E
import Registry.Api.Models.Item as Item exposing (Item, ItemDetail, Kind(..))
import Registry.Api.Models.Translation as Translation exposing (Translation)
import Registry.Api.Models.User as User exposing (ApiKey, User)
import Registry.Data.AppState as AppState exposing (AppState)
import Time
import Uuid exposing (Uuid)



-- THE SERVER


{-| Which version of the registry's server is running, for the About dialog.
-}
getBuildInfo : AppState -> ToMsg BuildInfo msg -> Cmd msg
getBuildInfo appState =
    Request.get (AppState.toServerInfo appState) "" BuildInfo.decoder



-- WHAT IS PUBLISHED


path : Kind -> String
path kind =
    case kind of
        KnowledgeModel ->
            "/knowledge-model-packages"

        DocumentTemplate ->
            "/document-templates"

        Locale ->
            "/locales"

        MailTemplateLocale ->
            "/mail-template-locales"


getItems : AppState -> Kind -> ToMsg (List Item) msg -> Cmd msg
getItems appState kind =
    Request.get (AppState.toServerInfo appState) (path kind) (D.list Item.decoder)


{-| One version, by its `id:version`.
-}
getItem : AppState -> Kind -> String -> ToMsg ItemDetail msg -> Cmd msg
getItem appState kind reference =
    Request.get (AppState.toServerInfo appState) (path kind ++ "/" ++ reference) (Item.detailDecoder kind)


{-| The newest translation of each language of a kind.
-}
getTranslations : AppState -> Kind -> ToMsg (List Translation) msg -> Cmd msg
getTranslations appState kind =
    Request.get (AppState.toServerInfo appState) (path kind) (D.list Translation.decoder)


{-| A translation's PO file, which anybody may download.
-}
translationPoUrl : AppState -> Kind -> Translation -> String
translationPoUrl appState kind translation =
    appState.apiUrl ++ path kind ++ "/" ++ translation.code ++ "/" ++ translation.version ++ "/po"



-- SIGNING IN AND THE ACCOUNT


{-| The session's token and when it ends. A token without an end is kept as
one that ends in the distant future.
-}
postToken : AppState -> { email : String, password : String } -> ToMsg { token : String, expiresAt : Time.Posix } msg -> Cmd msg
postToken appState credentials =
    let
        body =
            E.object
                [ ( "email", E.string credentials.email )
                , ( "password", E.string credentials.password )
                ]

        decoder =
            D.map2 (\token expiresAt -> { token = token, expiresAt = expiresAt })
                (D.field "token" D.string)
                (D.field "expiresAt" (D.map (Maybe.withDefault never) (D.nullable D.datetime)))
    in
    Request.post (AppState.toServerInfo appState) "/tokens" decoder body


never : Time.Posix
never =
    -- 9999-12-31
    Time.millisToPosix 253402214400000


postUser : AppState -> { email : String, firstName : String, lastName : String, password : String } -> ToMsg () msg -> Cmd msg
postUser appState form =
    Request.postWhatever (AppState.toServerInfo appState)
        "/users"
        (E.object
            [ ( "email", E.string form.email )
            , ( "firstName", E.string form.firstName )
            , ( "lastName", E.string form.lastName )
            , ( "password", E.string form.password )
            ]
        )


{-| The link in the e-mail sent after signing up.
-}
putUserActivation : AppState -> { uuid : String, hash : String } -> ToMsg () msg -> Cmd msg
putUserActivation appState { uuid, hash } =
    Request.putWhatever (AppState.toServerInfo appState)
        ("/users/" ++ uuid ++ "/state?hash=" ++ hash)
        (E.object [ ( "active", E.bool True ) ])


postForgottenPassword : AppState -> String -> ToMsg () msg -> Cmd msg
postForgottenPassword appState email =
    Request.postWhatever (AppState.toServerInfo appState)
        "/user-email-links"
        (E.object [ ( "type", E.string "ForgottenPasswordUserEmailLinkType" ), ( "email", E.string email ) ])


{-| The link in the e-mail sent for a forgotten password.
-}
putPassword : AppState -> { uuid : String, hash : String, password : String } -> ToMsg () msg -> Cmd msg
putPassword appState { uuid, hash, password } =
    Request.putWhatever (AppState.toServerInfo appState)
        ("/users/" ++ uuid ++ "/password?hash=" ++ hash)
        (E.object [ ( "password", E.string password ) ])


getCurrentUser : AppState -> ToMsg User msg -> Cmd msg
getCurrentUser appState =
    Request.get (AppState.toServerInfo appState) "/users/current" User.decoder


putCurrentUser : AppState -> { email : String, firstName : String, lastName : String } -> ToMsg User msg -> Cmd msg
putCurrentUser appState form =
    Request.put (AppState.toServerInfo appState)
        "/users/current"
        User.decoder
        (E.object
            [ ( "email", E.string form.email )
            , ( "firstName", E.string form.firstName )
            , ( "lastName", E.string form.lastName )
            ]
        )


putCurrentPassword : AppState -> String -> ToMsg () msg -> Cmd msg
putCurrentPassword appState password =
    Request.putWhatever (AppState.toServerInfo appState)
        "/users/current/password"
        (E.object [ ( "password", E.string password ) ])


getApiKeys : AppState -> ToMsg (List ApiKey) msg -> Cmd msg
getApiKeys appState =
    Request.get (AppState.toServerInfo appState) "/api-keys" (D.list User.apiKeyDecoder)


{-| The new key's value, the one time it is shown.
-}
postApiKey : AppState -> String -> ToMsg String msg -> Cmd msg
postApiKey appState name =
    Request.post (AppState.toServerInfo appState) "/api-keys" (D.field "token" D.string) (E.object [ ( "name", E.string name ) ])


deleteApiKey : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteApiKey appState uuid =
    Request.delete (AppState.toServerInfo appState) ("/api-keys/" ++ Uuid.toString uuid)



-- ACCOUNTS, FOR AN ADMINISTRATOR


{-| One page of the accounts, by e-mail, the first page being 0.
-}
getUsers : AppState -> Int -> ToMsg (Pagination User) msg -> Cmd msg
getUsers appState page =
    Request.get (AppState.toServerInfo appState)
        ("/users?sort=email,asc&size=" ++ String.fromInt usersPageSize ++ "&page=" ++ String.fromInt page)
        (Pagination.decoder "users" User.decoder)


usersPageSize : Int
usersPageSize =
    20


deleteUser : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteUser appState uuid =
    Request.delete (AppState.toServerInfo appState) ("/users/" ++ Uuid.toString uuid)
