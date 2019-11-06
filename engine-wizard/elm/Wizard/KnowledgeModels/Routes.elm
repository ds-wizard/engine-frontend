module Wizard.KnowledgeModels.Routes exposing (Route(..))


type Route
    = DetailRoute String
    | ImportRoute (Maybe String)
    | IndexRoute
