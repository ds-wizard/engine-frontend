module Common.Components.Questionnaire.Panel.Lists exposing
    ( viewTodos
    , viewUnanswered
    , viewWarnings
    )

{-| The three panels that are a list of questions and nothing else: what is still
unanswered, what is marked TODO, and what looks wrong.

Each is one `Html.Lazy` call over the locale and the list the model keeps
(`Rebuild` keeps them, §11.2), so a panel renders again only when what it shows
has changed. The list itself is `Panel.Group`.

-}

import Common.Api.Models.KnowledgeModel.Question as Question
import Common.Api.Models.Project.ProjectTodo exposing (ProjectTodo)
import Common.Api.Models.ProjectQuestionnaire exposing (QuestionnaireUnansweredQuestion, QuestionnaireWarning)
import Common.Components.Questionnaire.Panel.Group as Group
import Gettext exposing (Locale, gettext)
import Html exposing (Html)
import Html.Lazy as Lazy
import Ui.Icon as Icon


viewUnanswered : Locale -> List QuestionnaireUnansweredQuestion -> Html Group.Msg
viewUnanswered locale questions =
    Lazy.lazy2 viewUnansweredLazy locale questions


viewUnansweredLazy : Locale -> List QuestionnaireUnansweredQuestion -> Html Group.Msg
viewUnansweredLazy locale questions =
    Group.view
        { locale = locale
        , icon = Icon.solid "fa-pen"
        , empty =
            { title = gettext "Nothing left for now" locale
            , description = gettext "Every question that should be answered in this phase has been answered." locale
            }
        , items = List.map (\question -> { chapter = question.chapter, row = row question.path question.question }) questions
        }


viewTodos : Locale -> List ProjectTodo -> Html Group.Msg
viewTodos locale todos =
    Lazy.lazy2 viewTodosLazy locale todos


viewTodosLazy : Locale -> List ProjectTodo -> Html Group.Msg
viewTodosLazy locale todos =
    Group.view
        { locale = locale
        , icon = Icon.solid "fa-pen-to-square"
        , empty =
            { title = gettext "No TODOs" locale
            , description = gettext "Mark a question with TODO to come back to it later; the ones you mark are listed here." locale
            }
        , items = List.map (\todo -> { chapter = todo.chapter, row = row todo.path todo.question }) todos
        }


viewWarnings : Locale -> List QuestionnaireWarning -> Html Group.Msg
viewWarnings locale warnings =
    Lazy.lazy2 viewWarningsLazy locale warnings


viewWarningsLazy : Locale -> List QuestionnaireWarning -> Html Group.Msg
viewWarningsLazy locale warnings =
    Group.view
        { locale = locale
        , icon = Icon.solid "fa-triangle-exclamation"
        , empty =
            { title = gettext "No warnings" locale
            , description = gettext "An answer that does not look like what its question asks for — a date, an e-mail, a file that is gone — is listed here." locale
            }
        , items = List.map (\warning -> { chapter = warning.chapter, row = row warning.path warning.question }) warnings
        }


row : String -> Question.Question -> Group.Row
row path question =
    { path = path, title = Question.getTitle question, end = [] }
