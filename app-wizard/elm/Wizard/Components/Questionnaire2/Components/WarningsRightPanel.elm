module Wizard.Components.Questionnaire2.Components.WarningsRightPanel exposing
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
import Html.Events exposing (onClick)
import Html.Lazy as Lazy
import Wizard.Api.Models.KnowledgeModel.Question as Question
import Wizard.Api.Models.ProjectQuestionnaire exposing (QuestionnaireWarning)
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


view : Gettext.Locale -> List QuestionnaireWarning -> Html Msg
view locale warnings =
    Lazy.lazy2 viewLazy locale warnings


viewLazy : Gettext.Locale -> List QuestionnaireWarning -> Html Msg
viewLazy locale warnings =
    if List.isEmpty warnings then
        div [ class "questionnaireRightPanelList questionnaireRightPanelList--empty" ] <|
            [ illustratedMessage Undraw.feelingHappy (gettext "All warnings have been resolved!" locale) ]

    else
        let
            viewWarningGroup group =
                div []
                    [ strong [] [ text group.chapter.title ]
                    , ul [ class "fa-ul" ] (List.map viewTodo group.todos)
                    ]

            viewTodo todo =
                li []
                    [ span [ class "fa-li" ] [ fa "fas fa-exclamation-triangle" ]
                    , a [ onClick (ScrollToPath todo.path) ] [ text <| Question.getTitle todo.question ]
                    ]
        in
        div [ class "questionnaireRightPanelList" ] <|
            List.map viewWarningGroup (ProjectTodoGroup.groupTodos warnings)
