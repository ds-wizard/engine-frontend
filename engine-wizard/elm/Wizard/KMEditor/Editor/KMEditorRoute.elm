module Wizard.KMEditor.Editor.KMEditorRoute exposing (KMEditorRoute(..))

import Uuid exposing (Uuid)


type KMEditorRoute
    = Edit (Maybe Uuid)
    | QuestionTags
    | Preview
    | Settings
