module Wizard.Questionnaires.Common.QuestionChange exposing
    ( QuestionAddData
    , QuestionChange(..)
    , QuestionChangeData
    , getChapter
    , getQuestionUuid
    )

import Wizard.KMEditor.Common.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.KMEditor.Common.KnowledgeModel.Question as Question exposing (Question)


type QuestionChange
    = QuestionAdd QuestionAddData
    | QuestionChange QuestionChangeData


type alias QuestionAddData =
    { question : Question
    , chapter : Chapter
    }


type alias QuestionChangeData =
    { question : Question
    , originalQuestion : Question
    , chapter : Chapter
    }


getQuestionUuid : QuestionChange -> String
getQuestionUuid change =
    case change of
        QuestionAdd data ->
            Question.getUuid data.question

        QuestionChange data ->
            Question.getUuid data.question


getChapter : QuestionChange -> Chapter
getChapter change =
    case change of
        QuestionAdd data ->
            data.chapter

        QuestionChange data ->
            data.chapter
