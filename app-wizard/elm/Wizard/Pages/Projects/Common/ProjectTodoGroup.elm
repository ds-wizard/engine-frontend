module Wizard.Pages.Projects.Common.ProjectTodoGroup exposing
    ( QuestionnaireTodoGroup
    , groupTodos
    )

import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.Project.ProjectTodo exposing (ProjectTodo)


type alias QuestionnaireTodoGroup =
    { chapter : Chapter
    , todos : List ProjectTodo
    }


groupTodos : List ProjectTodo -> List QuestionnaireTodoGroup
groupTodos todos =
    let
        fold : ProjectTodo -> List QuestionnaireTodoGroup -> List QuestionnaireTodoGroup
        fold todo acc =
            if List.any (\group -> group.chapter.uuid == todo.chapter.uuid) acc then
                List.map
                    (\group ->
                        if group.chapter.uuid == todo.chapter.uuid then
                            { group | todos = group.todos ++ [ todo ] }

                        else
                            group
                    )
                    acc

            else
                acc ++ [ { chapter = todo.chapter, todos = [ todo ] } ]
    in
    List.foldl fold [] todos
