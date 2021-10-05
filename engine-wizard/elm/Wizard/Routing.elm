module Wizard.Routing exposing
    ( appRoute
    , cmdNavigate
    , cmdNavigateRaw
    , homeRoute
    , isAllowed
    , loginRoute
    , matchers
    , parseLocation
    , routeIfAllowed
    , signupRoute
    , toUrl
    )

import Browser.Navigation exposing (pushUrl)
import Shared.Locale exposing (lr)
import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, s)
import Wizard.Admin.Routing
import Wizard.Common.AppState exposing (AppState)
import Wizard.Documents.Routing
import Wizard.KMEditor.Routing
import Wizard.KnowledgeModels.Routing
import Wizard.Projects.Routing
import Wizard.Public.Routes
import Wizard.Public.Routing
import Wizard.Registry.Routing
import Wizard.Routes as Routes
import Wizard.Settings.Routing
import Wizard.Templates.Routing
import Wizard.Users.Routing


matchers : AppState -> Parser (Routes.Route -> b) b
matchers appState =
    let
        parsers =
            []
                ++ Wizard.Admin.Routing.parsers appState Routes.AdminRoute
                ++ Wizard.Documents.Routing.parsers appState Routes.DocumentsRoute
                ++ Wizard.KMEditor.Routing.parsers appState Routes.KMEditorRoute
                ++ Wizard.KnowledgeModels.Routing.parsers appState Routes.KnowledgeModelsRoute
                ++ Wizard.Projects.Routing.parsers appState Routes.ProjectsRoute
                ++ Wizard.Public.Routing.parsers appState Routes.PublicRoute
                ++ Wizard.Registry.Routing.parsers appState Routes.RegistryRoute
                ++ Wizard.Settings.Routing.parsers appState Routes.SettingsRoute
                ++ Wizard.Templates.Routing.parsers appState Routes.TemplatesRoute
                ++ Wizard.Users.Routing.parses Routes.UsersRoute
                ++ [ map Routes.DashboardRoute (s (lr "dashboard" appState))
                   ]
    in
    oneOf parsers


routeIfAllowed : AppState -> Routes.Route -> Routes.Route
routeIfAllowed appState route =
    if isAllowed route appState then
        route

    else
        Routes.NotAllowedRoute


isAllowed : Routes.Route -> AppState -> Bool
isAllowed route appState =
    case route of
        Routes.AdminRoute adminRoute ->
            Wizard.Admin.Routing.isAllowed adminRoute appState

        Routes.DashboardRoute ->
            True

        Routes.DocumentsRoute documentsRoute ->
            Wizard.Documents.Routing.isAllowed documentsRoute appState

        Routes.KMEditorRoute kmEditorRoute ->
            Wizard.KMEditor.Routing.isAllowed kmEditorRoute appState

        Routes.KnowledgeModelsRoute kmPackagesRoute ->
            Wizard.KnowledgeModels.Routing.isAllowed kmPackagesRoute appState

        Routes.ProjectsRoute plansRoute ->
            Wizard.Projects.Routing.isAllowed plansRoute appState

        Routes.PublicRoute _ ->
            True

        Routes.RegistryRoute registryRoute ->
            Wizard.Registry.Routing.isAllowed registryRoute appState

        Routes.SettingsRoute settingsRoute ->
            Wizard.Settings.Routing.isAllowed settingsRoute appState

        Routes.TemplatesRoute templatesRoute ->
            Wizard.Templates.Routing.isAllowed templatesRoute appState

        Routes.UsersRoute usersRoute ->
            Wizard.Users.Routing.isAllowed usersRoute appState

        Routes.NotFoundRoute ->
            True

        _ ->
            False


toUrl : AppState -> Routes.Route -> String
toUrl appState route =
    let
        parts =
            case route of
                Routes.AdminRoute adminRoute ->
                    Wizard.Admin.Routing.toUrl appState adminRoute

                Routes.DashboardRoute ->
                    [ lr "dashboard" appState ]

                Routes.DocumentsRoute documentsRoute ->
                    Wizard.Documents.Routing.toUrl appState documentsRoute

                Routes.KMEditorRoute kmEditorRoute ->
                    Wizard.KMEditor.Routing.toUrl appState kmEditorRoute

                Routes.KnowledgeModelsRoute kmPackagesRoute ->
                    Wizard.KnowledgeModels.Routing.toUrl appState kmPackagesRoute

                Routes.ProjectsRoute plansRoute ->
                    Wizard.Projects.Routing.toUrl appState plansRoute

                Routes.PublicRoute publicRoute ->
                    Wizard.Public.Routing.toUrl appState publicRoute

                Routes.RegistryRoute registryRoute ->
                    Wizard.Registry.Routing.toUrl appState registryRoute

                Routes.SettingsRoute settingsRoute ->
                    Wizard.Settings.Routing.toUrl appState settingsRoute

                Routes.TemplatesRoute templatesRoute ->
                    Wizard.Templates.Routing.toUrl appState templatesRoute

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


appRoute : Routes.Route
appRoute =
    Routes.DashboardRoute
