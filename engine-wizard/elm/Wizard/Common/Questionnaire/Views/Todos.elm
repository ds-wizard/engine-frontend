module Wizard.Common.Questionnaire.Views.Todos exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Locale exposing (l, lgx)
import Wizard.Common.Questionnaire.Models exposing (Model)
import Wizard.Common.Questionnaire.Msgs exposing (CustomFormMessage(..), Msg(..))
import Wizard.KMEditor.Common.KnowledgeModel.Question as Question
import Wizard.Questionnaires.Common.QuestionnaireDetail as QuestionnaireDetail
import Wizard.Questionnaires.Common.QuestionnaireTodo exposing (QuestionnaireTodo)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Questionnaire.Views.Todos"


view : AppState -> Model -> Html Msg
view appState model =
    div [ class "todos" ]
        [ h2 [] [ lgx "questionnaire.todos" appState ]
        , div [ class "list-group list-group-hover" ] (List.map viewTodo <| QuestionnaireDetail.getTodos model.questionnaire)
        ]


viewTodo : QuestionnaireTodo -> Html Msg
viewTodo todo =
    let
        isNested =
            (List.length <| String.split "." todo.path) > 2
    in
    a [ class "list-group-item flex-column", onClick <| ScrollToTodo todo ]
        [ div [] [ small [] [ text todo.chapter.title ] ]
        , p [ classList [ ( "nested", isNested ) ] ] [ text <| Question.getTitle todo.question ]
        ]
