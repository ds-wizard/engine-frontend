module Wizard.Projects.Common.AnswerChange exposing
    ( AnswerAddData
    , AnswerChange(..)
    , AnswerChangeData
    , getAnswerUuid
    )

import Wizard.Api.Models.KnowledgeModel.Answer exposing (Answer)


type AnswerChange
    = AnswerAdd AnswerAddData
    | AnswerChange AnswerChangeData


type alias AnswerAddData =
    { answer : Answer }


type alias AnswerChangeData =
    { answer : Answer
    , originalAnswer : Answer
    }


getAnswerUuid : AnswerChange -> String
getAnswerUuid change =
    case change of
        AnswerAdd data ->
            data.answer.uuid

        AnswerChange data ->
            data.answer.uuid
