module Wizard.Routing exposing
    ( cmdNavigate
    , parseLocation
    , routeIfAllowed
    , toUrl
    )

import Browser.Navigation exposing (pushUrl)
import Common.Data.PaginationQueryString as PaginationQueryString
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, s)
import Url.Parser.Query as Query
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dev.Routing
import Wizard.Pages.DocumentTemplateEditors.Routing
import Wizard.Pages.DocumentTemplates.Routing
import Wizard.Pages.Documents.Routing
import Wizard.Pages.KMEditor.Routing
import Wizard.Pages.KnowledgeModels.Routing
import Wizard.Pages.Locales.Routing
import Wizard.Pages.ProjectActions.Routing
import Wizard.Pages.ProjectFiles.Routing
import Wizard.Pages.ProjectImporters.Routing
import Wizard.Pages.Projects.Routing
import Wizard.Pages.Public.Routing
import Wizard.Pages.Registry.Routing
import Wizard.Pages.Settings.Routing
import Wizard.Pages.Tenants.Routing
import Wizard.Pages.Users.Routing
import Wizard.Routes as Routes exposing (commentsRouteResolvedFilterId)
import Wizard.Utils.Feature as Feature


matchers : AppState -> Parser (Routes.Route -> b) b
matchers appState =
    let
        commentsIndexRoute pqs mbResolved =
            Routes.CommentsRoute pqs mbResolved

        parsers =
            Wizard.Pages.Dev.Routing.parsers Routes.DevRoute
                ++ Wizard.Pages.Tenants.Routing.parsers Routes.TenantsRoute
                ++ Wizard.Pages.Documents.Routing.parsers Routes.DocumentsRoute
                ++ Wizard.Pages.DocumentTemplateEditors.Routing.parsers Routes.DocumentTemplateEditorsRoute
                ++ Wizard.Pages.DocumentTemplates.Routing.parsers Routes.DocumentTemplatesRoute
                ++ Wizard.Pages.KMEditor.Routing.parsers Routes.KMEditorRoute
                ++ Wizard.Pages.KnowledgeModels.Routing.parsers Routes.KnowledgeModelsRoute
                ++ Wizard.Pages.Locales.Routing.parsers appState Routes.LocalesRoute
                ++ Wizard.Pages.ProjectActions.Routing.parsers Routes.ProjectActionsRoute
                ++ Wizard.Pages.ProjectFiles.Routing.parsers Routes.ProjectFilesRoute
                ++ Wizard.Pages.ProjectImporters.Routing.parsers Routes.ProjectImportersRoute
                ++ Wizard.Pages.Projects.Routing.parsers Routes.ProjectsRoute
                ++ Wizard.Pages.Public.Routing.parsers appState Routes.PublicRoute
                ++ Wizard.Pages.Registry.Routing.parsers Routes.RegistryRoute
                ++ Wizard.Pages.Settings.Routing.parsers appState Routes.SettingsRoute
                ++ Wizard.Pages.Users.Routing.parsers appState Routes.UsersRoute
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
            Wizard.Pages.Dev.Routing.isAllowed adminRoute appState

        Routes.DashboardRoute ->
            True

        Routes.DocumentsRoute documentsRoute ->
            Wizard.Pages.Documents.Routing.isAllowed documentsRoute appState

        Routes.DocumentTemplateEditorsRoute _ ->
            Wizard.Pages.DocumentTemplateEditors.Routing.isAllowed appState

        Routes.DocumentTemplatesRoute templatesRoute ->
            Wizard.Pages.DocumentTemplates.Routing.isAllowed templatesRoute appState

        Routes.KMEditorRoute kmEditorRoute ->
            Wizard.Pages.KMEditor.Routing.isAllowed kmEditorRoute appState

        Routes.KnowledgeModelsRoute kmPackagesRoute ->
            Wizard.Pages.KnowledgeModels.Routing.isAllowed kmPackagesRoute appState

        Routes.KnowledgeModelSecretsRoute ->
            Feature.knowledgeModelSecrets appState

        Routes.LocalesRoute localeRoute ->
            Wizard.Pages.Locales.Routing.isAllowed localeRoute appState

        Routes.ProjectActionsRoute _ ->
            Wizard.Pages.ProjectActions.Routing.isAllowed appState

        Routes.ProjectFilesRoute _ ->
            Wizard.Pages.ProjectFiles.Routing.isAllowed appState

        Routes.ProjectImportersRoute _ ->
            Wizard.Pages.ProjectImporters.Routing.isAllowed appState

        Routes.ProjectsRoute projectsRoute ->
            Wizard.Pages.Projects.Routing.isAllowed projectsRoute appState

        Routes.PublicRoute _ ->
            True

        Routes.RegistryRoute registryRoute ->
            Wizard.Pages.Registry.Routing.isAllowed registryRoute appState

        Routes.SettingsRoute settingsRoute ->
            Wizard.Pages.Settings.Routing.isAllowed settingsRoute appState

        Routes.TenantsRoute tenantsRoute ->
            Wizard.Pages.Tenants.Routing.isAllowed tenantsRoute appState

        Routes.UsersRoute usersRoute ->
            Wizard.Pages.Users.Routing.isAllowed usersRoute appState

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
                    Wizard.Pages.Dev.Routing.toUrl adminRoute

                Routes.DashboardRoute ->
                    [ "dashboard" ]

                Routes.DocumentsRoute documentsRoute ->
                    Wizard.Pages.Documents.Routing.toUrl documentsRoute

                Routes.DocumentTemplateEditorsRoute templatesRoute ->
                    Wizard.Pages.DocumentTemplateEditors.Routing.toUrl templatesRoute

                Routes.DocumentTemplatesRoute templatesRoute ->
                    Wizard.Pages.DocumentTemplates.Routing.toUrl templatesRoute

                Routes.KMEditorRoute kmEditorRoute ->
                    Wizard.Pages.KMEditor.Routing.toUrl kmEditorRoute

                Routes.KnowledgeModelsRoute kmPackagesRoute ->
                    Wizard.Pages.KnowledgeModels.Routing.toUrl kmPackagesRoute

                Routes.KnowledgeModelSecretsRoute ->
                    [ "knowledge-model-secrets" ]

                Routes.LocalesRoute localeRoute ->
                    Wizard.Pages.Locales.Routing.toUrl localeRoute

                Routes.ProjectActionsRoute projectActionsRoute ->
                    Wizard.Pages.ProjectActions.Routing.toUrl projectActionsRoute

                Routes.ProjectFilesRoute projectFilesRoute ->
                    Wizard.Pages.ProjectFiles.Routing.toUrl projectFilesRoute

                Routes.ProjectImportersRoute projectImportersRoute ->
                    Wizard.Pages.ProjectImporters.Routing.toUrl projectImportersRoute

                Routes.ProjectsRoute projectsRoute ->
                    Wizard.Pages.Projects.Routing.toUrl projectsRoute

                Routes.PublicRoute publicRoute ->
                    Wizard.Pages.Public.Routing.toUrl publicRoute

                Routes.RegistryRoute registryRoute ->
                    Wizard.Pages.Registry.Routing.toUrl registryRoute

                Routes.SettingsRoute settingsRoute ->
                    Wizard.Pages.Settings.Routing.toUrl settingsRoute

                Routes.TenantsRoute tenantsRoute ->
                    Wizard.Pages.Tenants.Routing.toUrl tenantsRoute

                Routes.UsersRoute usersRoute ->
                    Wizard.Pages.Users.Routing.toUrl usersRoute

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
