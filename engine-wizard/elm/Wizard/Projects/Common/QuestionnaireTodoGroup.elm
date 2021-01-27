module Wizard.Projects.Common.QuestionnaireTodoGroup exposing
    ( QuestionnaireTodoGroup
    , groupTodos
    )

import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.Questionnaire.QuestionnaireTodo exposing (QuestionnaireTodo)


type alias QuestionnaireTodoGroup =
    { chapter : Chapter
    , todos : List QuestionnaireTodo
    }


groupTodos : List QuestionnaireTodo -> List QuestionnaireTodoGroup
groupTodos todos =
    let
        fold : QuestionnaireTodo -> List QuestionnaireTodoGroup -> List QuestionnaireTodoGroup
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
