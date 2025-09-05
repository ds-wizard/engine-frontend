module Wizard.Pages.Users.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Data.UuidOrCurrent as UuidOrCurrent
import Flip exposing (flip)
import List.Utils as List
import Url.Parser exposing ((</>), Parser, map, s)
import Url.Parser.Query as Query
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Users.Edit.UserEditRoutes as UserEditRoute
import Wizard.Pages.Users.Routes exposing (Route(..), indexRouteRoleFilterId)
import Wizard.Utils.Feature as Feature


moduleRoot : String
moduleRoot =
    "users"


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        wrappedIndexRoute pqs q =
            wrapRoute <| IndexRoute pqs q

        userRoutes =
            if Admin.isEnabled appState.config.admin then
                []

            else
                [ map (wrapRoute <| CreateRoute) (s moduleRoot </> s "create")
                , map (PaginationQueryString.wrapRoute1 wrappedIndexRoute (Just "lastName")) (PaginationQueryString.parser1 (s moduleRoot) (Query.string indexRouteRoleFilterId))
                , map (wrapRoute << flip EditRoute UserEditRoute.Password) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "password")
                , map (wrapRoute << flip EditRoute UserEditRoute.Language) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "language")
                ]
                    |> List.insertIf (map (wrapRoute << flip EditRoute UserEditRoute.Tours) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "tours")) appState.config.features.toursEnabled
    in
    [ map (wrapRoute << flip EditRoute UserEditRoute.Profile) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser)
    , map (wrapRoute << flip EditRoute UserEditRoute.ApiKeys) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "api-keys")
    , map (wrapRoute << flip EditRoute UserEditRoute.AppKeys) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "app-keys")
    , map (wrapRoute << flip EditRoute UserEditRoute.ActiveSessions) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "active-sessions")
    , map (wrapRoute << flip EditRoute UserEditRoute.SubmissionSettings) (s moduleRoot </> s "edit" </> UuidOrCurrent.parser </> s "submission-settings")
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

                UserEditRoute.Tours ->
                    editBase ++ [ "tours" ]

                UserEditRoute.ApiKeys ->
                    editBase ++ [ "api-keys" ]

                UserEditRoute.AppKeys ->
                    editBase ++ [ "app-keys" ]

                UserEditRoute.ActiveSessions ->
                    editBase ++ [ "active-sessions" ]

                UserEditRoute.SubmissionSettings ->
                    editBase ++ [ "submission-settings" ]

        IndexRoute paginationQueryString mbRole ->
            let
                params =
                    PaginationQueryString.filterParams [ ( indexRouteRoleFilterId, mbRole ) ]
            in
            [ moduleRoot ++ PaginationQueryString.toUrlWith params paginationQueryString ]


isAllowed : Route -> AppState -> Bool
isAllowed route appState =
    case route of
        CreateRoute ->
            Feature.usersCreate appState

        EditRoute uuidOrCurrent subroute ->
            if Admin.isEnabled appState.config.admin then
                UuidOrCurrent.isCurrent uuidOrCurrent

            else
                case subroute of
                    UserEditRoute.Language ->
                        Feature.userEditLanguage appState uuidOrCurrent

                    UserEditRoute.Tours ->
                        Feature.userEditTours appState uuidOrCurrent

                    UserEditRoute.ApiKeys ->
                        Feature.userEditApiKeys appState uuidOrCurrent

                    UserEditRoute.AppKeys ->
                        Feature.userEditAppKeys appState uuidOrCurrent

                    UserEditRoute.ActiveSessions ->
                        Feature.userEditActiveSessions appState uuidOrCurrent

                    UserEditRoute.SubmissionSettings ->
                        Feature.userEditSubmissionSettings appState uuidOrCurrent

                    _ ->
                        Feature.userEdit appState uuidOrCurrent

        IndexRoute _ _ ->
            Feature.usersView appState
