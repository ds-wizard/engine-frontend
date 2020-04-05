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
    | KnowledgeModelRegistryRoute
    | QuestionnairesRoute
    | SubmissionRoute


defaultRoute : Route
defaultRoute =
    OrganizationRoute
