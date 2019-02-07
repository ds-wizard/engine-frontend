module KMEditor.Editor.Msgs exposing
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
import Jwt
import KMEditor.Common.Models.Entities exposing (..)
import Reorderable
import SplitPane


type Msg
    = GetKnowledgeModelCompleted (Result Jwt.JwtError KnowledgeModel)
    | GetMetricsCompleted (Result Jwt.JwtError (List Metric))
    | GetLevelsCompleted (Result Jwt.JwtError (List Level))
    | ToggleOpen String
    | SetActiveEditor String
    | EditorMsg EditorMsg
    | ReorderableMsg Reorderable.Msg
    | CloseAlert
    | Submit
    | SubmitCompleted (Result Jwt.JwtError String)
    | Discard
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


type TagEditorMsg
    = TagFormMsg Form.Msg
    | DeleteTag String


type ChapterEditorMsg
    = ChapterFormMsg Form.Msg
    | DeleteChapter String
    | ReorderQuestions (List String)
    | AddQuestion


type QuestionEditorMsg
    = QuestionFormMsg Form.Msg
    | DeleteQuestion String
    | ReorderAnswers (List String)
    | AddAnswer
    | ReorderAnswerItemTemplateQuestions (List String)
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
