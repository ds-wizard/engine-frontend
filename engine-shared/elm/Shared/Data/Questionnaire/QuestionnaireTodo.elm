module Shared.Data.Questionnaire.QuestionnaireTodo exposing (QuestionnaireTodo)

import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Question exposing (Question)


type alias QuestionnaireTodo =
    { chapter : Chapter
    , question : Question
    , path : String
    }
