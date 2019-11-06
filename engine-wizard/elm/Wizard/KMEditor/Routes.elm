module Wizard.KMEditor.Routes exposing (Route(..))


type Route
    = CreateRoute (Maybe String)
    | EditorRoute String
    | IndexRoute
    | MigrationRoute String
    | PublishRoute String
