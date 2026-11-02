module Common.Api.Models.Project.ProjectTodo exposing (ProjectTodo)

import Common.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Common.Api.Models.KnowledgeModel.Question exposing (Question)


type alias ProjectTodo =
    { chapter : Chapter
    , question : Question
    , path : String
    }
