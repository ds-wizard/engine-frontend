module Wizard.Pages.Settings.Routes exposing
    ( Route(..)
    , defaultRoute
    , isPluginsSubroute
    )

import Uuid exposing (Uuid)


type Route
    = OrganizationRoute
    | AuthenticationRoute
    | PrivacyAndSupportRoute
    | FeaturesRoute
    | PluginsRoute
    | PluginSettingsRoute Uuid
    | DashboardAndLoginScreenRoute
    | LookAndFeelRoute
    | RegistryRoute
    | ProjectsRoute
    | SubmissionRoute
    | KnowledgeModelsRoute
    | UsageRoute


defaultRoute : Bool -> Route
defaultRoute adminEnabled =
    if adminEnabled then
        DashboardAndLoginScreenRoute

    else
        OrganizationRoute


isPluginsSubroute : Route -> Bool
isPluginsSubroute route =
    case route of
        PluginsRoute ->
            True

        PluginSettingsRoute _ ->
            True

        _ ->
            False
