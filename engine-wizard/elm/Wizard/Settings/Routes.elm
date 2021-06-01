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
    | KnowledgeModelsRoute


defaultRoute : Route
defaultRoute =
    OrganizationRoute
