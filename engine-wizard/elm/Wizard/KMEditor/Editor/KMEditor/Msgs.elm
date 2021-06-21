module Wizard.KMEditor.Editor.KMEditor.Msgs exposing
    ( AnswerEditorMsg(..)
    , ChapterEditorMsg(..)
    , ChoiceEditorMsg(..)
    , EditorMsg(..)
    , ExpertEditorMsg(..)
    , IntegrationEditorMsg(..)
    , KMEditorMsg(..)
    , MetricEditorMsg(..)
    , Msg(..)
    , PhaseEditorMsg(..)
    , QuestionEditorMsg(..)
    , ReferenceEditorMsg(..)
    , TagEditorMsg(..)
    )

import Form
import Reorderable
import SplitPane
import ValueList
import Wizard.KMEditor.Editor.KMEditor.Components.MoveModal as MoveModal


type Msg
    = ToggleOpen String
    | SetActiveEditor String
    | EditorMsg EditorMsg
    | ReorderableMsg Reorderable.Msg
    | CloseAlert
    | PaneMsg SplitPane.Msg
    | CopyUuid String
    | OpenMoveModal
    | MoveModalMsg MoveModal.Msg
    | TreeExpandAll
    | TreeCollapseAll


type EditorMsg
    = KMEditorMsg KMEditorMsg
    | MetricEditorMsg MetricEditorMsg
    | PhaseEditorMsg PhaseEditorMsg
    | TagEditorMsg TagEditorMsg
    | IntegrationEditorMsg IntegrationEditorMsg
    | ChapterEditorMsg ChapterEditorMsg
    | QuestionEditorMsg QuestionEditorMsg
    | AnswerEditorMsg AnswerEditorMsg
    | ChoiceEditorMsg ChoiceEditorMsg
    | ReferenceEditorMsg ReferenceEditorMsg
    | ExpertEditorMsg ExpertEditorMsg


type KMEditorMsg
    = KMEditorFormMsg Form.Msg
    | ReorderChapters (List String)
    | AddChapter
    | ReorderMetrics (List String)
    | ReorderPhases (List String)
    | ReorderTags (List String)
    | ReorderIntegrations (List String)
    | AddMetric
    | AddPhase
    | AddTag
    | AddIntegration


type ChapterEditorMsg
    = ChapterFormMsg Form.Msg
    | DeleteChapter String
    | ReorderQuestions (List String)
    | AddQuestion


type MetricEditorMsg
    = MetricFormMsg Form.Msg
    | DeleteMetric String


type PhaseEditorMsg
    = PhaseFormMsg Form.Msg
    | DeletePhase String


type TagEditorMsg
    = TagFormMsg Form.Msg
    | DeleteTag String


type IntegrationEditorMsg
    = IntegrationFormMsg Form.Msg
    | ToggleDeleteConfirm Bool
    | DeleteIntegration String
    | PropsListMsg ValueList.Msg


type QuestionEditorMsg
    = QuestionFormMsg Form.Msg
    | AddQuestionTag String
    | RemoveQuestionTag String
    | DeleteQuestion String
    | ReorderAnswers (List String)
    | AddAnswer
    | ReorderChoices (List String)
    | AddChoice
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


type ChoiceEditorMsg
    = ChoiceFormMsg Form.Msg
    | DeleteChoice String


type ReferenceEditorMsg
    = ReferenceFormMsg Form.Msg
    | DeleteReference String


type ExpertEditorMsg
    = ExpertFormMsg Form.Msg
    | DeleteExpert String
