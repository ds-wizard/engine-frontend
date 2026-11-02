module Common.Pages.KMEditor.Editor.Routes exposing (KMEditorRoute(..))

import Uuid exposing (Uuid)


type KMEditorRoute
    = Edit (Maybe Uuid)
    | Phases
    | QuestionTags
    | Preview
    | Settings
