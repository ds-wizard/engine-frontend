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
    | QuestionnairesRoute
    | SubmissionRoute
    | TemplateRoute


defaultRoute : Route
defaultRoute =
    OrganizationRoute
