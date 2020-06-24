module Shared.Data.Questionnaire.QuestionnaireTodo exposing
    ( QuestionnaireTodo
    , getSelectorPath
    )

import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Question exposing (Question)


type alias QuestionnaireTodo =
    { chapter : Chapter
    , question : Question
    , path : String
    }


getSelectorPath : QuestionnaireTodo -> String
getSelectorPath =
    .path >> String.split "." >> List.drop 1 >> String.join "."
