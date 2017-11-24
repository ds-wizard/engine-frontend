module KnowledgeModels.Editor.Msgs exposing (..)

import Form
import Jwt
import KnowledgeModels.Editor.Models.Editors exposing (..)
import KnowledgeModels.Editor.Models.Entities exposing (..)
import Reorderable


type Msg
    = GetKnowledgeModelCompleted (Result Jwt.JwtError KnowledgeModel)
    | Edit KnowledgeModelMsg
    | ReorderableMsg Reorderable.Msg
    | SaveCompleted (Result Jwt.JwtError String)


type KnowledgeModelMsg
    = KnowledgeModelFormMsg Form.Msg
    | ViewChapter String
    | AddChapter
    | DeleteChapter String
    | ReorderChapterList (List ChapterEditor)
    | ChapterMsg String ChapterMsg


type ChapterMsg
    = ChapterFormMsg Form.Msg
    | ChapterCancel
    | ViewQuestion String
    | AddChapterQuestion
    | DeleteChapterQuestion String
    | ReorderQuestionList (List QuestionEditor)
    | ChapterQuestionMsg String QuestionMsg


type QuestionMsg
    = QuestionFormMsg Form.Msg
    | QuestionCancel
    | QuestionDelete
    | AnswerMsg Int AnswerMsg
    | ReferenceMsg Int ReferenceMsg
    | ExpertMsg Int ExpertMsg


type AnswerMsg
    = AnswerFormMsg Form.Msg
    | AnswerQuestionMsg Int QuestionMsg


type ReferenceMsg
    = ReferenceFormMsg Form.Msg


type ExpertMsg
    = ExpertFormMsg Form.Msg
