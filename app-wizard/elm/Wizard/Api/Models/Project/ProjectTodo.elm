module Wizard.Api.Models.Project.ProjectTodo exposing (ProjectTodo)

import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Question exposing (Question)


type alias ProjectTodo =
    { chapter : Chapter
    , question : Question
    , path : String
    }
