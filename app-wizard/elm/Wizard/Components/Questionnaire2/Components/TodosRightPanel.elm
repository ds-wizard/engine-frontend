module Wizard.Components.Questionnaire2.Components.TodosRightPanel exposing
    ( Msg
    , UpdateConfig
    , update
    , view
    )

import Common.Components.FontAwesome exposing (fa)
import Common.Components.Undraw as Undraw
import Gettext exposing (gettext)
import Html exposing (Html, a, div, li, span, strong, text, ul)
import Html.Attributes exposing (class)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Lazy as Lazy
import Wizard.Api.Models.KnowledgeModel.Question as Question
import Wizard.Api.Models.Project.ProjectTodo exposing (ProjectTodo)
import Wizard.Components.Html exposing (illustratedMessage)
import Wizard.Pages.Projects.Common.ProjectTodoGroup as ProjectTodoGroup


type Msg
    = ScrollToPath String


type alias UpdateConfig msg =
    { scrollToPathCmd : String -> Cmd msg }


update : UpdateConfig msg -> Msg -> Cmd msg
update config msg =
    case msg of
        ScrollToPath path ->
            config.scrollToPathCmd path


view : Gettext.Locale -> List ProjectTodo -> Html Msg
view locale todos =
    Lazy.lazy2 viewLazy locale todos


viewLazy : Gettext.Locale -> List ProjectTodo -> Html Msg
viewLazy locale todos =
    let
        viewTodoGroup group =
            div []
                [ strong [] [ text group.chapter.title ]
                , ul [ class "fa-ul" ] (List.map viewTodo group.todos)
                ]

        viewTodo todo =
            li []
                [ span [ class "fa-li" ] [ fa "fas fa-edit" ]
                , a [ onClick (ScrollToPath todo.path) ] [ text <| Question.getTitle todo.question ]
                ]
    in
    if List.isEmpty todos then
        div
            [ class "questionnaireRightPanelList questionnaireRightPanelList--empty"
            , dataCy "questionnaire_todos"
            ]
        <|
            [ illustratedMessage Undraw.feelingHappy (gettext "All TODOs have been completed." locale) ]

    else
        div
            [ class "questionnaireRightPanelList"
            , dataCy "questionnaire_todos"
            ]
        <|
            List.map viewTodoGroup (ProjectTodoGroup.groupTodos todos)
