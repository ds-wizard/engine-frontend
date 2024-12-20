module Wizard.Settings.Routes exposing
    ( Route(..)
    , defaultRoute
    )


type Route
    = OrganizationRoute
    | AuthenticationRoute
    | PrivacyAndSupportRoute
    | DashboardAndLoginScreenRoute
    | LookAndFeelRoute
    | RegistryRoute
    | ProjectsRoute
    | SubmissionRoute
    | KnowledgeModelsRoute
    | UsageRoute


defaultRoute : Route
defaultRoute =
    OrganizationRoute
