module Common.Questionnaire.Models.QuestionnaireFeature exposing
    ( QuestionnaireFeature
    , feedback
    , feedbackEnabled
    , summaryReport
    , summaryReportEnabled
    , todoList
    , todoListEnabled
    , todos
    , todosEnabled
    )

import Common.AppState exposing (AppState)


type QuestionnaireFeature
    = Feedback
    | SummaryReport
    | Todos
    | TodoList


feedback : QuestionnaireFeature
feedback =
    Feedback


summaryReport : QuestionnaireFeature
summaryReport =
    SummaryReport


todos : QuestionnaireFeature
todos =
    Todos


todoList : QuestionnaireFeature
todoList =
    TodoList


feedbackEnabled : AppState -> List QuestionnaireFeature -> Bool
feedbackEnabled appState features =
    if appState.config.feedbackEnabled then
        enabled Feedback features

    else
        False


summaryReportEnabled : List QuestionnaireFeature -> Bool
summaryReportEnabled =
    enabled SummaryReport


todosEnabled : List QuestionnaireFeature -> Bool
todosEnabled =
    enabled Todos


todoListEnabled : List QuestionnaireFeature -> Bool
todoListEnabled =
    enabled TodoList


enabled : QuestionnaireFeature -> List QuestionnaireFeature -> Bool
enabled feature =
    List.any ((==) feature)
