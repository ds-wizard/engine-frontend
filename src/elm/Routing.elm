module Routing exposing
    ( appRoute
    , cmdNavigate
    , homeRoute
    , isAllowed
    , loginRoute
    , matchers
    , parseLocation
    , questionnaireDemoRoute
    , routeIfAllowed
    , signupRoute
    , toUrl
    )

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import Browser.Navigation exposing (Key, pushUrl)
import Common.AppState exposing (AppState)
import Common.Locale exposing (lr)
import KMEditor.Routing
import KnowledgeModels.Routing
import Public.Routes
import Public.Routing
import Questionnaires.Routing
import Routes
import Url exposing (Url)
import Url.Parser exposing (..)
import Users.Routing


matchers : AppState -> Parser (Routes.Route -> b) b
matchers appState =
    let
        parsers =
            []
                ++ Questionnaires.Routing.parses appState Routes.QuestionnairesRoute
                ++ KMEditor.Routing.parsers appState Routes.KMEditorRoute
                ++ KnowledgeModels.Routing.parsers appState Routes.KnowledgeModelsRoute
                ++ Public.Routing.parsers appState Routes.PublicRoute
                ++ Users.Routing.parses Routes.UsersRoute
                ++ [ map Routes.DashboardRoute (s (lr "dashboard" appState))
                   , map Routes.OrganizationRoute (s (lr "organization" appState))
                   ]
    in
    oneOf parsers


routeIfAllowed : Maybe JwtToken -> Routes.Route -> Routes.Route
routeIfAllowed maybeJwt route =
    if isAllowed route maybeJwt then
        route

    else
        Routes.NotAllowedRoute


isAllowed : Routes.Route -> Maybe JwtToken -> Bool
isAllowed route maybeJwt =
    case route of
        Routes.DashboardRoute ->
            True

        Routes.QuestionnairesRoute dsPlannerRoute ->
            Questionnaires.Routing.isAllowed dsPlannerRoute maybeJwt

        Routes.KMEditorRoute kmEditorRoute ->
            KMEditor.Routing.isAllowed kmEditorRoute maybeJwt

        Routes.KnowledgeModelsRoute kmPackagesRoute ->
            KnowledgeModels.Routing.isAllowed kmPackagesRoute maybeJwt

        Routes.OrganizationRoute ->
            hasPerm maybeJwt Perm.organization

        Routes.PublicRoute _ ->
            True

        Routes.UsersRoute usersRoute ->
            Users.Routing.isAllowed usersRoute maybeJwt

        Routes.NotFoundRoute ->
            True

        _ ->
            False


toUrl : AppState -> Routes.Route -> String
toUrl appState route =
    let
        parts =
            case route of
                Routes.DashboardRoute ->
                    [ lr "dashboard" appState ]

                Routes.QuestionnairesRoute questionnairesRoute ->
                    Questionnaires.Routing.toUrl appState questionnairesRoute

                Routes.KMEditorRoute kmEditorRoute ->
                    KMEditor.Routing.toUrl appState kmEditorRoute

                Routes.KnowledgeModelsRoute kmPackagesRoute ->
                    KnowledgeModels.Routing.toUrl appState kmPackagesRoute

                Routes.OrganizationRoute ->
                    [ lr "organization" appState ]

                Routes.PublicRoute publicRoute ->
                    Public.Routing.toUrl appState publicRoute

                Routes.UsersRoute usersRoute ->
                    Users.Routing.toUrl usersRoute

                _ ->
                    []
    in
    "/"
        ++ String.join "/" parts
        |> String.split "/?"
        |> String.join "?"


parseLocation : AppState -> Url -> Routes.Route
parseLocation appState url =
    case Url.Parser.parse (matchers appState) url of
        Just route ->
            route

        Nothing ->
            Routes.NotFoundRoute


cmdNavigate : AppState -> Routes.Route -> Cmd msg
cmdNavigate appState =
    pushUrl appState.key << toUrl appState


homeRoute : Routes.Route
homeRoute =
    Routes.PublicRoute Public.Routes.LoginRoute


loginRoute : Routes.Route
loginRoute =
    Routes.PublicRoute Public.Routes.LoginRoute


signupRoute : Routes.Route
signupRoute =
    Routes.PublicRoute Public.Routes.SignupRoute


questionnaireDemoRoute : Routes.Route
questionnaireDemoRoute =
    Routes.PublicRoute Public.Routes.QuestionnaireRoute


appRoute : Routes.Route
appRoute =
    Routes.DashboardRoute
