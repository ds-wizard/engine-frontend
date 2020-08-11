module Wizard.KMEditor.Editor.Preview.Msgs exposing (Msg(..))

import Wizard.Common.Components.Questionnaire as Questionnaire


type Msg
    = QuestionnaireMsg Questionnaire.Msg
    | AddTag String
    | RemoveTag String
    | SelectAllTags
    | SelectNoneTags
