module Wizard.Projects.Detail.Components.Todos exposing (view)

import Html exposing (Html, a, div, h2, p, small, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Shared.Data.KnowledgeModel.Question as Question
import Shared.Data.Questionnaire.QuestionnaireTodo exposing (QuestionnaireTodo)
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Locale exposing (l, lgx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Page as Page
import Wizard.Projects.Detail.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Detail.Components.Todos"


view : AppState -> QuestionnaireDetail -> Html Msg
view appState questionnaire =
    let
        todos =
            QuestionnaireDetail.getTodos questionnaire

        content =
            if List.isEmpty todos then
                [ Page.illustratedMessage
                    { image = "feeling_happy"
                    , heading = l_ "doneTitle" appState
                    , lines = [ l_ "doneText" appState ]
                    }
                ]

            else
                [ h2 [] [ lgx "questionnaire.todos" appState ]
                , div [ class "list-group list-group-hover" ]
                    (List.map viewTodo todos)
                ]
    in
    div [ class "Plans__Detail__Content Plans__Detail__Content--Todos" ]
        [ div [ class "container" ] content ]


viewTodo : QuestionnaireTodo -> Html Msg
viewTodo todo =
    let
        isNested =
            (List.length <| String.split "." todo.path) > 2
    in
    a
        [ class "list-group-item flex-column"
        , onClick (ScrollToTodo todo)
        ]
        [ div [] [ small [] [ text todo.chapter.title ] ]
        , p [ classList [ ( "nested", isNested ) ] ] [ text <| Question.getTitle todo.question ]
        ]
