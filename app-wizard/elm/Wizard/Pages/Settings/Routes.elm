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
    | UsageRoute


defaultRoute : Bool -> Bool -> Route
defaultRoute adminEnabled pluginsAvailable =
    if adminEnabled then
        if pluginsAvailable then
            PluginsRoute

        else
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
