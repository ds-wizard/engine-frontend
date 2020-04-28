module Wizard.Routing exposing
    ( appRoute
    , cmdNavigate
    , cmdNavigateRaw
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

import Browser.Navigation exposing (Key, pushUrl)
import Shared.Locale exposing (lr)
import Url exposing (Url)
import Url.Parser exposing (..)
import Wizard.Auth.Permission as Perm exposing (hasPerm)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.JwtToken exposing (JwtToken)
import Wizard.Documents.Routing
import Wizard.KMEditor.Routing
import Wizard.KnowledgeModels.Routing
import Wizard.Public.Routes
import Wizard.Public.Routing
import Wizard.Questionnaires.Routing
import Wizard.Registry.Routing
import Wizard.Routes as Routes
import Wizard.Settings.Routing
import Wizard.Users.Routing


matchers : AppState -> Parser (Routes.Route -> b) b
matchers appState =
    let
        parsers =
            []
                ++ Wizard.Documents.Routing.parsers appState Routes.DocumentsRoute
                ++ Wizard.KMEditor.Routing.parsers appState Routes.KMEditorRoute
                ++ Wizard.KnowledgeModels.Routing.parsers appState Routes.KnowledgeModelsRoute
                ++ Wizard.Public.Routing.parsers appState Routes.PublicRoute
                ++ Wizard.Questionnaires.Routing.parsers appState Routes.QuestionnairesRoute
                ++ Wizard.Registry.Routing.parsers appState Routes.RegistryRoute
                ++ Wizard.Settings.Routing.parsers appState Routes.SettingsRoute
                ++ Wizard.Users.Routing.parses Routes.UsersRoute
                ++ [ map Routes.DashboardRoute (s (lr "dashboard" appState))
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

        Routes.DocumentsRoute documentsRoute ->
            Wizard.Documents.Routing.isAllowed documentsRoute maybeJwt

        Routes.KMEditorRoute kmEditorRoute ->
            Wizard.KMEditor.Routing.isAllowed kmEditorRoute maybeJwt

        Routes.KnowledgeModelsRoute kmPackagesRoute ->
            Wizard.KnowledgeModels.Routing.isAllowed kmPackagesRoute maybeJwt

        Routes.PublicRoute _ ->
            True

        Routes.QuestionnairesRoute questionnaireRoute ->
            Wizard.Questionnaires.Routing.isAllowed questionnaireRoute maybeJwt

        Routes.RegistryRoute registryRoute ->
            Wizard.Registry.Routing.isAllowed registryRoute maybeJwt

        Routes.SettingsRoute settingsRoute ->
            Wizard.Settings.Routing.isAllowed settingsRoute maybeJwt

        Routes.UsersRoute usersRoute ->
            Wizard.Users.Routing.isAllowed usersRoute maybeJwt

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

                Routes.DocumentsRoute documentsRoute ->
                    Wizard.Documents.Routing.toUrl appState documentsRoute

                Routes.QuestionnairesRoute questionnairesRoute ->
                    Wizard.Questionnaires.Routing.toUrl appState questionnairesRoute

                Routes.KMEditorRoute kmEditorRoute ->
                    Wizard.KMEditor.Routing.toUrl appState kmEditorRoute

                Routes.KnowledgeModelsRoute kmPackagesRoute ->
                    Wizard.KnowledgeModels.Routing.toUrl appState kmPackagesRoute

                Routes.PublicRoute publicRoute ->
                    Wizard.Public.Routing.toUrl appState publicRoute

                Routes.RegistryRoute registryRoute ->
                    Wizard.Registry.Routing.toUrl appState registryRoute

                Routes.SettingsRoute settingsRoute ->
                    Wizard.Settings.Routing.toUrl appState settingsRoute

                Routes.UsersRoute usersRoute ->
                    Wizard.Users.Routing.toUrl usersRoute

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


cmdNavigateRaw : AppState -> String -> Cmd msg
cmdNavigateRaw appState =
    pushUrl appState.key


homeRoute : Routes.Route
homeRoute =
    Routes.PublicRoute <| Wizard.Public.Routes.LoginRoute Nothing


loginRoute : Maybe String -> Routes.Route
loginRoute originalUrl =
    Routes.PublicRoute <| Wizard.Public.Routes.LoginRoute originalUrl


signupRoute : Routes.Route
signupRoute =
    Routes.PublicRoute Wizard.Public.Routes.SignupRoute


questionnaireDemoRoute : Routes.Route
questionnaireDemoRoute =
    Routes.PublicRoute Wizard.Public.Routes.QuestionnaireRoute


appRoute : Routes.Route
appRoute =
    Routes.DashboardRoute
