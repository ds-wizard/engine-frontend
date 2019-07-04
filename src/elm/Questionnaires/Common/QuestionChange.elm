module Questionnaires.Common.QuestionChange exposing
    ( QuestionAddData
    , QuestionChange(..)
    , QuestionChangeData
    , getChapter
    , getQuestionUuid
    )

import KMEditor.Common.Models.Entities as Entities exposing (Chapter, Question)


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
            Entities.getQuestionUuid data.question

        QuestionChange data ->
            Entities.getQuestionUuid data.question


getChapter : QuestionChange -> Chapter
getChapter change =
    case change of
        QuestionAdd data ->
            data.chapter

        QuestionChange data ->
            data.chapter
