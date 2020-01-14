module Wizard.KMEditor.Editor.Preview.Msgs exposing (Msg(..))

import Wizard.Common.Questionnaire.Msgs


type Msg
    = QuestionnaireMsg Wizard.Common.Questionnaire.Msgs.Msg
    | AddTag String
    | RemoveTag String
    | SelectAllTags
    | SelectNoneTags
