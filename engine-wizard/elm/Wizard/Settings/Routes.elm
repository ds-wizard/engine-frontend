module Wizard.Settings.Routes exposing
    ( Route(..)
    , defaultRoute
    )


type Route
    = AffiliationRoute
    | AuthRoute
    | ClientRoute
    | FeaturesRoute
    | InfoRoute
    | OrganizationRoute


defaultRoute : Route
defaultRoute =
    FeaturesRoute
