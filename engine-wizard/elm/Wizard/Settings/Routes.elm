module Wizard.Settings.Routes exposing
    ( Route(..)
    , defaultRoute
    )


type Route
    = OrganizationRoute
    | AuthenticationRoute
    | PrivacyAndSupportRoute
    | DashboardRoute
    | LookAndFeelRoute
    | RegistryRoute
    | ProjectsRoute
    | SubmissionRoute
    | TemplateRoute
    | KnowledgeModelsRoute
    | UsageRoute


defaultRoute : Route
defaultRoute =
    OrganizationRoute
