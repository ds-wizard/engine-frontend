module Wizard.KMEditor.Routes exposing (Route(..))

import Uuid exposing (Uuid)


type Route
    = CreateRoute (Maybe String)
    | EditorRoute Uuid
    | IndexRoute
    | MigrationRoute Uuid
    | PublishRoute Uuid
