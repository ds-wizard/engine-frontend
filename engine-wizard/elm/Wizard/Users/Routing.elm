module Wizard.Users.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Dict
import Shared.Common.UuidOrCurrent as UuidOrCurrent
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Utils exposing (dictFromMaybeList, flip)
import Url.Parser exposing ((</>), Parser, map, s)
import Url.Parser.Query as Query
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.Users.Edit.UserEditRoutes as UserEditRoute
import Wizard.Users.Routes exposing (Route(..), indexRouteRoleFilterId)


moduleRoot : String
moduleRoot =
    "users"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    let
        wrappedIndexRoute pqs q =
            wrapRoute <| IndexRoute pqs q
    in
    [ map (wrapRoute <| CreateRoute) (s moduleRoot </> s "create")
    , map (wrapRoute << flip EditRoute UserEditRoute.Profile) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser)
    , map (wrapRoute << flip EditRoute UserEditRoute.Password) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "password")
    , map (wrapRoute << flip EditRoute UserEditRoute.ApiKeys) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "api-keys")
    , map (wrapRoute << flip EditRoute UserEditRoute.ActiveSessions) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "active-sessions")
    , map (wrapRoute << flip EditRoute UserEditRoute.SubmissionSettings) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "submission-settings")
    , map (PaginationQueryString.wrapRoute1 wrappedIndexRoute (Just "lastName")) (PaginationQueryString.parser1 (s moduleRoot) (Query.string indexRouteRoleFilterId))
    ]


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

                UserEditRoute.ApiKeys ->
                    editBase ++ [ "api-keys" ]

                UserEditRoute.ActiveSessions ->
                    editBase ++ [ "active-sessions" ]

                UserEditRoute.SubmissionSettings ->
                    editBase ++ [ "submission-settings" ]

        IndexRoute paginationQueryString mbRole ->
            let
                params =
                    Dict.toList <| dictFromMaybeList [ ( indexRouteRoleFilterId, mbRole ) ]
            in
            [ moduleRoot ++ PaginationQueryString.toUrlWith params paginationQueryString ]


isAllowed : Route -> AppState -> Bool
isAllowed route appState =
    case route of
        CreateRoute ->
            Feature.usersCreate appState

        EditRoute uuidOrCurrent subroute ->
            case subroute of
                UserEditRoute.ApiKeys ->
                    Feature.userEditApiKeys appState uuidOrCurrent

                UserEditRoute.ActiveSessions ->
                    Feature.userEditActiveSessions appState uuidOrCurrent

                UserEditRoute.SubmissionSettings ->
                    Feature.userEditSubmissionSettings appState uuidOrCurrent

                _ ->
                    Feature.userEdit appState uuidOrCurrent

        IndexRoute _ _ ->
            Feature.usersView appState
