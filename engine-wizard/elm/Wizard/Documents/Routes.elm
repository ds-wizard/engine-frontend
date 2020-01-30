module Wizard.Documents.Routes exposing (Route(..))


type Route
    = CreateRoute (Maybe String)
    | IndexRoute (Maybe String)
