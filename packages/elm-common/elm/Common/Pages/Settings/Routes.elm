module Common.Pages.Settings.Routes exposing
    ( LocalesPage(..)
    , Route(..)
    , defaultRoute
    , isAllowed
    , isPluginsSubroute
    , parsers
    , toUrl
    )

import Common.Data.Feature as Feature exposing (AppStateLike)
import Common.Data.LocaleKind as LocaleKind exposing (LocaleKind(..))
import Common.Plugins.PluginMetadata exposing (PluginMetadata)
import List.Extensions as List
import Url.Parser exposing ((</>), Parser, map, s)
import Url.Parser.Extensions as Parser
import Uuid exposing (Uuid)


type Route
    = AuthenticationRoute
    | UsersRoute
    | OpenIdRoute
    | OpenIdCreateRoute
    | OpenIdDetailRoute Uuid
    | RolesRoute
    | RoleCreateRoute
    | RoleDetailRoute Uuid
    | SupportRoute
    | FeaturesRoute
    | PluginsRoute
    | PluginSettingsRoute Uuid
    | LoginScreenRoute
    | DashboardAndMenuRoute
    | LookAndFeelRoute
    | RegistryRoute
    | ProjectsRoute
    | SubmissionRoute
    | UsageRoute
    | LocalesRoute LocaleKind LocalesPage


{-| The pages of the translations of either kind: the list, the
upload, and the registry's (where the server has one).
-}
type LocalesPage
    = LocalesListPage
    | LocalesUploadPage
    | LocalesRegistryPage


{-| Where "Settings" leads: the first section the person may change, or the
roles for somebody who may only manage those.
-}
defaultRoute : AppStateLike a -> Route
defaultRoute appState =
    if Feature.settings appState then
        AuthenticationRoute

    else if Feature.workspaceSettings appState then
        DashboardAndMenuRoute

    else
        RolesRoute


isPluginsSubroute : Route -> Bool
isPluginsSubroute route =
    case route of
        PluginsRoute ->
            True

        PluginSettingsRoute _ ->
            True

        _ ->
            False


moduleRoot : String
moduleRoot =
    "settings"


parsers : AppStateLike { b | pluginMetadata : List PluginMetadata } -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        pluginsAvilable =
            not (List.isEmpty appState.pluginMetadata)
    in
    []
        |> List.insertIf (map (wrapRoute <| AuthenticationRoute) (s moduleRoot </> s "authentication")) True
        |> List.insertIf (map (wrapRoute <| UsersRoute) (s moduleRoot </> s "users")) True
        |> List.insertIf (map (wrapRoute <| OpenIdRoute) (s moduleRoot </> s "open-id")) True
        |> List.insertIf (map (wrapRoute <| OpenIdCreateRoute) (s moduleRoot </> s "open-id" </> s "create")) True
        |> List.insertIf (map (wrapRoute << OpenIdDetailRoute) (s moduleRoot </> s "open-id" </> Parser.uuid)) True
        |> List.insertIf (map (wrapRoute <| RolesRoute) (s moduleRoot </> s "roles")) True
        |> List.insertIf (map (wrapRoute <| RoleCreateRoute) (s moduleRoot </> s "roles" </> s "create")) True
        |> List.insertIf (map (wrapRoute << RoleDetailRoute) (s moduleRoot </> s "roles" </> Parser.uuid)) True
        |> List.insertIf (map (wrapRoute <| SupportRoute) (s moduleRoot </> s "support")) True
        |> List.insertIf (map (wrapRoute <| FeaturesRoute) (s moduleRoot </> s "features")) True
        |> List.insertIf (map (wrapRoute <| PluginsRoute) (s moduleRoot </> s "plugins")) pluginsAvilable
        |> List.insertIf (map (wrapRoute << PluginSettingsRoute) (s moduleRoot </> s "plugins" </> Parser.uuid)) pluginsAvilable
        |> List.insertIf (map (wrapRoute <| LoginScreenRoute) (s moduleRoot </> s "login-screen")) True
        |> List.insertIf (map (wrapRoute <| DashboardAndMenuRoute) (s moduleRoot </> s "dashboard-and-menu")) True
        |> List.insertIf (map (wrapRoute <| LookAndFeelRoute) (s moduleRoot </> s "look-and-feel")) True
        |> List.insertIf (map (wrapRoute <| RegistryRoute) (s moduleRoot </> s "registry")) (Feature.registry appState)
        |> List.insertIf (map (wrapRoute <| ProjectsRoute) (s moduleRoot </> s "projects")) True
        |> List.insertIf (map (wrapRoute <| SubmissionRoute) (s moduleRoot </> s "submission")) True
        |> List.insertIf (map (wrapRoute <| UsageRoute) (s moduleRoot </> s "usage")) True
        |> (\routes -> routes ++ List.concatMap (localesParsers wrapRoute) [ UiLocales, MailTemplateLocales ])


localesParsers : (Route -> a) -> LocaleKind -> List (Parser (a -> c) c)
localesParsers wrapRoute kind =
    let
        base =
            s moduleRoot </> s (LocaleKind.urlSegment kind)
    in
    [ map (wrapRoute (LocalesRoute kind LocalesListPage)) base
    , map (wrapRoute (LocalesRoute kind LocalesUploadPage)) (base </> s "upload")
    , map (wrapRoute (LocalesRoute kind LocalesRegistryPage)) (base </> s "registry")
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        AuthenticationRoute ->
            [ moduleRoot, "authentication" ]

        UsersRoute ->
            [ moduleRoot, "users" ]

        OpenIdRoute ->
            [ moduleRoot, "open-id" ]

        OpenIdCreateRoute ->
            [ moduleRoot, "open-id", "create" ]

        OpenIdDetailRoute openIdUuid ->
            [ moduleRoot, "open-id", Uuid.toString openIdUuid ]

        RolesRoute ->
            [ moduleRoot, "roles" ]

        RoleCreateRoute ->
            [ moduleRoot, "roles", "create" ]

        RoleDetailRoute roleUuid ->
            [ moduleRoot, "roles", Uuid.toString roleUuid ]

        SupportRoute ->
            [ moduleRoot, "support" ]

        FeaturesRoute ->
            [ moduleRoot, "features" ]

        PluginsRoute ->
            [ moduleRoot, "plugins" ]

        PluginSettingsRoute pluginUuid ->
            [ moduleRoot, "plugins", Uuid.toString pluginUuid ]

        LoginScreenRoute ->
            [ moduleRoot, "login-screen" ]

        DashboardAndMenuRoute ->
            [ moduleRoot, "dashboard-and-menu" ]

        LookAndFeelRoute ->
            [ moduleRoot, "look-and-feel" ]

        RegistryRoute ->
            [ moduleRoot, "registry" ]

        ProjectsRoute ->
            [ moduleRoot, "projects" ]

        SubmissionRoute ->
            [ moduleRoot, "submission" ]

        UsageRoute ->
            [ moduleRoot, "usage" ]

        LocalesRoute kind page ->
            moduleRoot
                :: LocaleKind.urlSegment kind
                :: (case page of
                        LocalesListPage ->
                            []

                        LocalesUploadPage ->
                            [ "upload" ]

                        LocalesRegistryPage ->
                            [ "registry" ]
                   )


isAllowed : Route -> AppStateLike a -> Bool
isAllowed route appState =
    case route of
        RolesRoute ->
            Feature.roles appState

        RoleCreateRoute ->
            Feature.roles appState

        RoleDetailRoute _ ->
            Feature.roles appState

        RegistryRoute ->
            Feature.settings appState && Feature.registry appState

        LocalesRoute _ LocalesRegistryPage ->
            Feature.settings appState && Feature.registry appState

        DashboardAndMenuRoute ->
            Feature.workspaceSettings appState

        ProjectsRoute ->
            Feature.workspaceSettings appState

        SupportRoute ->
            Feature.workspaceSettings appState

        SubmissionRoute ->
            Feature.workspaceSettings appState

        _ ->
            Feature.settings appState
