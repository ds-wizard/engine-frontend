module Wizard.Routing exposing
    ( cmdNavigate
    , parseLocation
    , routeIfAllowed
    , toUrl
    )

import Browser.Navigation exposing (pushUrl)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, s)
import Url.Parser.Query as Query
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.Dev.Routing
import Wizard.DocumentTemplateEditors.Routing
import Wizard.DocumentTemplates.Routing
import Wizard.Documents.Routing
import Wizard.KMEditor.Routing
import Wizard.KnowledgeModels.Routing
import Wizard.Locales.Routing
import Wizard.ProjectActions.Routing
import Wizard.ProjectFiles.Routing
import Wizard.ProjectImporters.Routing
import Wizard.Projects.Routing
import Wizard.Public.Routing
import Wizard.Registry.Routing
import Wizard.Routes as Routes exposing (commentsRouteResolvedFilterId)
import Wizard.Settings.Routing
import Wizard.Tenants.Routing
import Wizard.Users.Routing


matchers : AppState -> Parser (Routes.Route -> b) b
matchers appState =
    let
        commentsIndexRoute pqs mbResolved =
            Routes.CommentsRoute pqs mbResolved

        parsers =
            Wizard.Dev.Routing.parsers Routes.DevRoute
                ++ Wizard.Tenants.Routing.parsers Routes.TenantsRoute
                ++ Wizard.Documents.Routing.parsers Routes.DocumentsRoute
                ++ Wizard.DocumentTemplateEditors.Routing.parsers Routes.DocumentTemplateEditorsRoute
                ++ Wizard.DocumentTemplates.Routing.parsers Routes.DocumentTemplatesRoute
                ++ Wizard.KMEditor.Routing.parsers Routes.KMEditorRoute
                ++ Wizard.KnowledgeModels.Routing.parsers Routes.KnowledgeModelsRoute
                ++ Wizard.Locales.Routing.parsers appState Routes.LocalesRoute
                ++ Wizard.ProjectActions.Routing.parsers Routes.ProjectActionsRoute
                ++ Wizard.ProjectFiles.Routing.parsers Routes.ProjectFilesRoute
                ++ Wizard.ProjectImporters.Routing.parsers Routes.ProjectImportersRoute
                ++ Wizard.Projects.Routing.parsers Routes.ProjectsRoute
                ++ Wizard.Public.Routing.parsers appState Routes.PublicRoute
                ++ Wizard.Registry.Routing.parsers Routes.RegistryRoute
                ++ Wizard.Settings.Routing.parsers appState Routes.SettingsRoute
                ++ Wizard.Users.Routing.parsers appState Routes.UsersRoute
                ++ [ map Routes.DashboardRoute (s "dashboard")
                   , map Routes.KnowledgeModelSecretsRoute (s "knowledge-model-secrets")
                   , map (PaginationQueryString.wrapRoute1 commentsIndexRoute (Just "updatedAt,desc")) (PaginationQueryString.parser1 (s "comments") (Query.string commentsRouteResolvedFilterId))
                   ]

        parsersWithPrefix =
            List.map (\p -> map identity (s "wizard" </> p)) parsers
    in
    oneOf parsersWithPrefix


routeIfAllowed : AppState -> Routes.Route -> Routes.Route
routeIfAllowed appState route =
    if isAllowed route appState then
        route

    else
        Routes.NotAllowedRoute


isAllowed : Routes.Route -> AppState -> Bool
isAllowed route appState =
    case route of
        Routes.CommentsRoute _ _ ->
            True

        Routes.DevRoute adminRoute ->
            Wizard.Dev.Routing.isAllowed adminRoute appState

        Routes.DashboardRoute ->
            True

        Routes.DocumentsRoute documentsRoute ->
            Wizard.Documents.Routing.isAllowed documentsRoute appState

        Routes.DocumentTemplateEditorsRoute _ ->
            Wizard.DocumentTemplateEditors.Routing.isAllowed appState

        Routes.DocumentTemplatesRoute templatesRoute ->
            Wizard.DocumentTemplates.Routing.isAllowed templatesRoute appState

        Routes.KMEditorRoute kmEditorRoute ->
            Wizard.KMEditor.Routing.isAllowed kmEditorRoute appState

        Routes.KnowledgeModelsRoute kmPackagesRoute ->
            Wizard.KnowledgeModels.Routing.isAllowed kmPackagesRoute appState

        Routes.KnowledgeModelSecretsRoute ->
            Feature.knowledgeModelSecrets appState

        Routes.LocalesRoute localeRoute ->
            Wizard.Locales.Routing.isAllowed localeRoute appState

        Routes.ProjectActionsRoute _ ->
            Wizard.ProjectActions.Routing.isAllowed appState

        Routes.ProjectFilesRoute _ ->
            Wizard.ProjectFiles.Routing.isAllowed appState

        Routes.ProjectImportersRoute _ ->
            Wizard.ProjectImporters.Routing.isAllowed appState

        Routes.ProjectsRoute projectsRoute ->
            Wizard.Projects.Routing.isAllowed projectsRoute appState

        Routes.PublicRoute _ ->
            True

        Routes.RegistryRoute registryRoute ->
            Wizard.Registry.Routing.isAllowed registryRoute appState

        Routes.SettingsRoute settingsRoute ->
            Wizard.Settings.Routing.isAllowed settingsRoute appState

        Routes.TenantsRoute tenantsRoute ->
            Wizard.Tenants.Routing.isAllowed tenantsRoute appState

        Routes.UsersRoute usersRoute ->
            Wizard.Users.Routing.isAllowed usersRoute appState

        Routes.NotFoundRoute ->
            True

        _ ->
            False


toUrl : Routes.Route -> String
toUrl route =
    let
        parts =
            case route of
                Routes.CommentsRoute pqs mbResolved ->
                    let
                        params =
                            PaginationQueryString.filterParams
                                [ ( commentsRouteResolvedFilterId, mbResolved ) ]
                    in
                    [ "comments", PaginationQueryString.toUrlWith params pqs ]

                Routes.DevRoute adminRoute ->
                    Wizard.Dev.Routing.toUrl adminRoute

                Routes.DashboardRoute ->
                    [ "dashboard" ]

                Routes.DocumentsRoute documentsRoute ->
                    Wizard.Documents.Routing.toUrl documentsRoute

                Routes.DocumentTemplateEditorsRoute templatesRoute ->
                    Wizard.DocumentTemplateEditors.Routing.toUrl templatesRoute

                Routes.DocumentTemplatesRoute templatesRoute ->
                    Wizard.DocumentTemplates.Routing.toUrl templatesRoute

                Routes.KMEditorRoute kmEditorRoute ->
                    Wizard.KMEditor.Routing.toUrl kmEditorRoute

                Routes.KnowledgeModelsRoute kmPackagesRoute ->
                    Wizard.KnowledgeModels.Routing.toUrl kmPackagesRoute

                Routes.KnowledgeModelSecretsRoute ->
                    [ "knowledge-model-secrets" ]

                Routes.LocalesRoute localeRoute ->
                    Wizard.Locales.Routing.toUrl localeRoute

                Routes.ProjectActionsRoute projectActionsRoute ->
                    Wizard.ProjectActions.Routing.toUrl projectActionsRoute

                Routes.ProjectFilesRoute projectFilesRoute ->
                    Wizard.ProjectFiles.Routing.toUrl projectFilesRoute

                Routes.ProjectImportersRoute projectImportersRoute ->
                    Wizard.ProjectImporters.Routing.toUrl projectImportersRoute

                Routes.ProjectsRoute projectsRoute ->
                    Wizard.Projects.Routing.toUrl projectsRoute

                Routes.PublicRoute publicRoute ->
                    Wizard.Public.Routing.toUrl publicRoute

                Routes.RegistryRoute registryRoute ->
                    Wizard.Registry.Routing.toUrl registryRoute

                Routes.SettingsRoute settingsRoute ->
                    Wizard.Settings.Routing.toUrl settingsRoute

                Routes.TenantsRoute tenantsRoute ->
                    Wizard.Tenants.Routing.toUrl tenantsRoute

                Routes.UsersRoute usersRoute ->
                    Wizard.Users.Routing.toUrl usersRoute

                _ ->
                    []
    in
    "/wizard/"
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
    pushUrl appState.key << toUrl
