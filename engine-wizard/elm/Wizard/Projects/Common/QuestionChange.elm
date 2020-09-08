module Wizard.Projects.Common.QuestionChange exposing
    ( QuestionAddData
    , QuestionChange(..)
    , QuestionChangeData
    , QuestionMoveData
    , getChapter
    , getQuestionUuid
    )

import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question)


type QuestionChange
    = QuestionAdd QuestionAddData
    | QuestionChange QuestionChangeData
    | QuestionMove QuestionMoveData


type alias QuestionAddData =
    { question : Question
    , chapter : Chapter
    }


type alias QuestionChangeData =
    { question : Question
    , originalQuestion : Question
    , chapter : Chapter
    }


type alias QuestionMoveData =
    { question : Question
    , chapter : Chapter
    }


getQuestionUuid : QuestionChange -> String
getQuestionUuid change =
    case change of
        QuestionAdd data ->
            Question.getUuid data.question

        QuestionChange data ->
            Question.getUuid data.question

        QuestionMove data ->
            Question.getUuid data.question


getChapter : QuestionChange -> Chapter
getChapter change =
    case change of
        QuestionAdd data ->
            data.chapter

        QuestionChange data ->
            data.chapter

        QuestionMove data ->
            data.chapter
