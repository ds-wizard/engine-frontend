module Wizard.Settings.Routes exposing
    ( Route(..)
    , defaultRoute
    )


type Route
    = AffiliationRoute
    | ClientRoute
    | FeaturesRoute
    | InfoRoute
    | OrganizationRoute


defaultRoute : Route
defaultRoute =
    FeaturesRoute
