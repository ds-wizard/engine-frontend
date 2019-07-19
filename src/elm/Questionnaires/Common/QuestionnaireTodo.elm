module Questionnaires.Common.QuestionnaireTodo exposing (QuestionnaireTodo, getSelectorPath)

import KMEditor.Common.Models.Entities exposing (Chapter, Question)


type alias QuestionnaireTodo =
    { chapter : Chapter
    , question : Question
    , path : String
    }


getSelectorPath : QuestionnaireTodo -> String
getSelectorPath =
    .path >> String.split "." >> List.drop 1 >> String.join "."
