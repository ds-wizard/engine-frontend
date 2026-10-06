module Wizard.Components.Questionnaire.Components.UnansweredQuestionsRightPanel exposing
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
import Wizard.Api.Models.ProjectQuestionnaire exposing (QuestionnaireUnansweredQuestion)
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


view : Gettext.Locale -> List QuestionnaireUnansweredQuestion -> Html Msg
view locale unansweredQuestions =
    Lazy.lazy2 viewLazy locale unansweredQuestions


viewLazy : Gettext.Locale -> List QuestionnaireUnansweredQuestion -> Html Msg
viewLazy locale unansweredQuestions =
    let
        viewChapterGroup group =
            div []
                [ strong [] [ text group.chapter.title ]
                , ul [ class "fa-ul" ] (List.map viewUnansweredQuestion group.todos)
                ]

        viewUnansweredQuestion unansweredQuestion =
            li []
                [ span [ class "fa-li" ] [ fa "fas fa-pen" ]
                , a [ onClick (ScrollToPath unansweredQuestion.path) ] [ text <| Question.getTitle unansweredQuestion.question ]
                ]
    in
    if List.isEmpty unansweredQuestions then
        div
            [ class "questionnaireRightPanelList questionnaireRightPanelList--empty"
            , dataCy "questionnaire_unanswered-questions"
            ]
        <|
            [ illustratedMessage Undraw.feelingHappy (gettext "All questions that should be answered now have been answered." locale) ]

    else
        div
            [ class "questionnaireRightPanelList"
            , dataCy "questionnaire_unanswered-questions"
            ]
        <|
            List.map viewChapterGroup (ProjectTodoGroup.groupTodos unansweredQuestions)
