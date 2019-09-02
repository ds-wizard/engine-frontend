module Questionnaires.Common.QuestionnaireTodo exposing (QuestionnaireTodo, getSelectorPath)

import KMEditor.Common.KnowledgeModel.Chapter exposing (Chapter)
import KMEditor.Common.KnowledgeModel.Question exposing (Question)


type alias QuestionnaireTodo =
    { chapter : Chapter
    , question : Question
    , path : String
    }


getSelectorPath : QuestionnaireTodo -> String
getSelectorPath =
    .path >> String.split "." >> List.drop 1 >> String.join "."
