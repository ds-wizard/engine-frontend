module KnowledgeModels.Editor.Msgs exposing (..)

{-|

@docs Msg, KnowledgeModelMsg, ChapterMsg, QuestionMsg, AnswerMsg, ReferenceMsg, ExpertMsg

-}

import Form
import Jwt
import KnowledgeModels.Editor.Models.Editors exposing (..)
import KnowledgeModels.Editor.Models.Entities exposing (..)
import Reorderable


{-| -}
type Msg
    = GetKnowledgeModelCompleted (Result Jwt.JwtError KnowledgeModel)
    | Edit KnowledgeModelMsg
    | ReorderableMsg Reorderable.Msg
    | SaveCompleted (Result Jwt.JwtError String)


{-| -}
type KnowledgeModelMsg
    = KnowledgeModelFormMsg Form.Msg
    | ViewChapter String
    | AddChapter
    | DeleteChapter String
    | ReorderChapterList (List ChapterEditor)
    | ChapterMsg String ChapterMsg


{-| -}
type ChapterMsg
    = ChapterFormMsg Form.Msg
    | ChapterCancel
    | ViewQuestion String
    | AddChapterQuestion
    | DeleteChapterQuestion String
    | ReorderQuestionList (List QuestionEditor)
    | ChapterQuestionMsg String QuestionMsg


{-| -}
type QuestionMsg
    = QuestionFormMsg Form.Msg
    | QuestionCancel
    | ViewAnswer String
    | AddAnswer
    | DeleteAnswer String
    | ReorderAnswerList (List AnswerEditor)
    | AnswerMsg String AnswerMsg
    | ViewReference String
    | AddReference
    | DeleteReference String
    | ReorderReferenceList (List ReferenceEditor)
    | ReferenceMsg String ReferenceMsg
    | ViewExpert String
    | AddExpert
    | DeleteExpert String
    | ReorderExpertList (List ExpertEditor)
    | ExpertMsg String ExpertMsg


{-| -}
type AnswerMsg
    = AnswerFormMsg Form.Msg
    | AnswerCancel
    | ViewFollowUpQuestion String
    | AddFollowUpQuestion
    | DeleteFollowUpQuestion String
    | ReorderFollowUpQuestionList (List QuestionEditor)
    | FollowUpQuestionMsg String QuestionMsg


{-| -}
type ReferenceMsg
    = ReferenceFormMsg Form.Msg
    | ReferenceCancel


{-| -}
type ExpertMsg
    = ExpertFormMsg Form.Msg
    | ExpertCancel
