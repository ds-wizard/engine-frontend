module Common.Pages.Users.Routes exposing
    ( Route(..)
    , indexRouteRoleFilterId
    , isAllowed
    , parsers
    , toUrl
    )

import Common.Api.Models.BootstrapConfig exposing (BootstrapConfig)
import Common.Data.Feature as Feature exposing (AppStateLike)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Data.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Common.Pages.Users.Edit.Routes as UserEditRoute exposing (UserEditRoute)
import Flip exposing (flip)
import List.Extensions as List
import Url.Parser exposing ((</>), Parser, map, s)
import Url.Parser.Extensions exposing (uuid)
import Url.Parser.Query as Query
import Uuid


type Route
    = CreateRoute
    | EditRoute UuidOrCurrent UserEditRoute
    | IndexRoute PaginationQueryString (Maybe String)


indexRouteRoleFilterId : String
indexRouteRoleFilterId =
    "role"


moduleRoot : String
moduleRoot =
    "users"


parsers : { b | config : BootstrapConfig } -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        wrappedIndexRoute pqs q =
            wrapRoute <| IndexRoute pqs q

        wrappedPluginRoute uuidOrCurrent pluginUuid =
            wrapRoute (EditRoute uuidOrCurrent (UserEditRoute.PluginSettings pluginUuid))

        userRoutes =
            [ map (wrapRoute <| CreateRoute) (s moduleRoot </> s "create")
            , map (PaginationQueryString.wrapRoute1 wrappedIndexRoute (Just "lastName")) (PaginationQueryString.parser1 (s moduleRoot) (Query.string indexRouteRoleFilterId))
            , map (wrapRoute << flip EditRoute UserEditRoute.Password) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "password")
            , map (wrapRoute << flip EditRoute UserEditRoute.ConnectedAccounts) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "connected-accounts")
            , map (wrapRoute << flip EditRoute UserEditRoute.Language) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "language")
            , map (wrapRoute << flip EditRoute UserEditRoute.ApiKeys) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "api-keys")
            , map (wrapRoute << flip EditRoute UserEditRoute.ActiveSessions) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "active-sessions")
            ]
                |> List.insertIf (map (wrapRoute << flip EditRoute UserEditRoute.Tours) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "tours")) appState.config.features.toursEnabled
    in
    [ map (wrapRoute << flip EditRoute UserEditRoute.Profile) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser)
    , map (wrapRoute << flip EditRoute UserEditRoute.SubmissionSettings) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "submission-settings")
    , map wrappedPluginRoute (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "plugins" </> uuid)
    ]
        ++ userRoutes


toUrl : Route -> List String
toUrl route =
    case route of
        CreateRoute ->
            [ moduleRoot, "create" ]

        EditRoute uuidOrCurrent subroute ->
            let
                editBase =
                    [ moduleRoot, "edit", UuidOrCurrent.toString uuidOrCurrent ]
            in
            case subroute of
                UserEditRoute.Profile ->
                    editBase

                UserEditRoute.Password ->
                    editBase ++ [ "password" ]

                UserEditRoute.Language ->
                    editBase ++ [ "language" ]

                UserEditRoute.ConnectedAccounts ->
                    editBase ++ [ "connected-accounts" ]

                UserEditRoute.Tours ->
                    editBase ++ [ "tours" ]

                UserEditRoute.ApiKeys ->
                    editBase ++ [ "api-keys" ]

                UserEditRoute.ActiveSessions ->
                    editBase ++ [ "active-sessions" ]

                UserEditRoute.SubmissionSettings ->
                    editBase ++ [ "submission-settings" ]

                UserEditRoute.PluginSettings pluginUuid ->
                    editBase ++ [ "plugins", Uuid.toString pluginUuid ]

        IndexRoute paginationQueryString mbRole ->
            let
                params =
                    PaginationQueryString.filterParams [ ( indexRouteRoleFilterId, mbRole ) ]
            in
            [ moduleRoot ++ PaginationQueryString.toUrlWith params paginationQueryString ]


isAllowed : Route -> AppStateLike a -> Bool
isAllowed route appState =
    case route of
        CreateRoute ->
            Feature.usersCreate appState

        EditRoute uuidOrCurrent subroute ->
            case subroute of
                UserEditRoute.Profile ->
                    Feature.userEdit appState uuidOrCurrent

                UserEditRoute.Password ->
                    Feature.userEdit appState uuidOrCurrent

                UserEditRoute.ConnectedAccounts ->
                    Feature.userEditConnectedAccounts appState uuidOrCurrent

                UserEditRoute.Language ->
                    Feature.userEditLanguage appState uuidOrCurrent

                UserEditRoute.Tours ->
                    Feature.userEditTours appState uuidOrCurrent

                UserEditRoute.ApiKeys ->
                    Feature.userEditApiKeys appState uuidOrCurrent

                UserEditRoute.ActiveSessions ->
                    Feature.userEditActiveSessions appState uuidOrCurrent

                UserEditRoute.SubmissionSettings ->
                    Feature.userEditSubmissionSettings appState uuidOrCurrent

                UserEditRoute.PluginSettings _ ->
                    Feature.userEditPlugins appState uuidOrCurrent

        IndexRoute _ _ ->
            Feature.usersView appState
