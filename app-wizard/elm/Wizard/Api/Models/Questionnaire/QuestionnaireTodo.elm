module Wizard.Api.Models.Questionnaire.QuestionnaireTodo exposing (QuestionnaireTodo)

import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Question exposing (Question)


type alias QuestionnaireTodo =
    { chapter : Chapter
    , question : Question
    , path : String
    }
