module Wizard.Pages.Settings.Routes exposing
    ( Route(..)
    , defaultRoute
    )


type Route
    = OrganizationRoute
    | AuthenticationRoute
    | PrivacyAndSupportRoute
    | FeaturesRoute
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
