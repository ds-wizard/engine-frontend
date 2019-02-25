module KMEditor.Editor.Preview.Msgs exposing (Msg(..))

import Common.Questionnaire.Msgs


type Msg
    = QuestionnaireMsg Common.Questionnaire.Msgs.Msg
    | AddTag String
    | RemoveTag String
    | SelectAllTags
    | SelectNoneTags
