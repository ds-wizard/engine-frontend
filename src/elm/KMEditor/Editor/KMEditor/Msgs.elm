module KMEditor.Editor.KMEditor.Msgs exposing
    ( AnswerEditorMsg(..)
    , ChapterEditorMsg(..)
    , EditorMsg(..)
    , ExpertEditorMsg(..)
    , KMEditorMsg(..)
    , Msg(..)
    , QuestionEditorMsg(..)
    , ReferenceEditorMsg(..)
    , TagEditorMsg(..)
    )

import Form
import Reorderable
import SplitPane


type Msg
    = ToggleOpen String
    | SetActiveEditor String
    | EditorMsg EditorMsg
    | ReorderableMsg Reorderable.Msg
    | CloseAlert
    | PaneMsg SplitPane.Msg


type EditorMsg
    = KMEditorMsg KMEditorMsg
    | TagEditorMsg TagEditorMsg
    | ChapterEditorMsg ChapterEditorMsg
    | QuestionEditorMsg QuestionEditorMsg
    | AnswerEditorMsg AnswerEditorMsg
    | ReferenceEditorMsg ReferenceEditorMsg
    | ExpertEditorMsg ExpertEditorMsg


type KMEditorMsg
    = KMEditorFormMsg Form.Msg
    | ReorderChapters (List String)
    | AddChapter
    | ReorderTags (List String)
    | AddTag


type ChapterEditorMsg
    = ChapterFormMsg Form.Msg
    | DeleteChapter String
    | ReorderQuestions (List String)
    | AddQuestion


type TagEditorMsg
    = TagFormMsg Form.Msg
    | DeleteTag String


type QuestionEditorMsg
    = QuestionFormMsg Form.Msg
    | AddQuestionTag String
    | RemoveQuestionTag String
    | DeleteQuestion String
    | ReorderAnswers (List String)
    | AddAnswer
    | ReorderItemQuestions (List String)
    | AddAnswerItemTemplateQuestion
    | ReorderReferences (List String)
    | AddReference
    | ReorderExperts (List String)
    | AddExpert


type AnswerEditorMsg
    = AnswerFormMsg Form.Msg
    | DeleteAnswer String
    | ReorderFollowUps (List String)
    | AddFollowUp


type ReferenceEditorMsg
    = ReferenceFormMsg Form.Msg
    | DeleteReference String


type ExpertEditorMsg
    = ExpertFormMsg Form.Msg
    | DeleteExpert String
