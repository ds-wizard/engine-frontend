module Common.Questionnaire.Views.Todos exposing (view)

import Common.Questionnaire.Models exposing (Model)
import Common.Questionnaire.Msgs exposing (CustomFormMessage(..), Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import KMEditor.Common.KnowledgeModel.Question as Question
import Questionnaires.Common.QuestionnaireDetail as QuestionnaireDetail
import Questionnaires.Common.QuestionnaireTodo exposing (QuestionnaireTodo)


view : Model -> Html Msg
view model =
    div [ class "todos" ]
        [ h2 [] [ text "TODOs" ]
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
