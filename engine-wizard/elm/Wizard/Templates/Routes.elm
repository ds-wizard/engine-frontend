module Wizard.Templates.Routes exposing (Route(..))


type Route
    = DetailRoute String
    | ImportRoute (Maybe String)
    | IndexRoute
