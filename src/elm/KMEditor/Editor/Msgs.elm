module KMEditor.Editor.Msgs exposing (..)

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
    | PaneMsg SplitPane.Msg
    | NoOp


type EditorMsg
    = KMEditorMsg KMEditorMsg
    | ChapterEditorMsg ChapterEditorMsg
    | QuestionEditorMsg QuestionEditorMsg
    | AnswerEditorMsg AnswerEditorMsg
    | ReferenceEditorMsg ReferenceEditorMsg
    | ExpertEditorMsg ExpertEditorMsg


type KMEditorMsg
    = KMEditorFormMsg Form.Msg
    | ReorderChapters (List String)
    | AddChapter


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
