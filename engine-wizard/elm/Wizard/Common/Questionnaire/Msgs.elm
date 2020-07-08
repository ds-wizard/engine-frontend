module Wizard.Common.Questionnaire.Msgs exposing (CustomFormMessage(..), Msg(..))

import Form
import Result exposing (Result)
import Shared.Data.Feedback exposing (Feedback)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.Questionnaire.QuestionnaireTodo exposing (QuestionnaireTodo)
import Shared.Data.SummaryReport exposing (SummaryReport)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.FormEngine.Model exposing (TypeHint)
import Wizard.Common.FormEngine.Msgs


type Msg
    = FormMsg (Wizard.Common.FormEngine.Msgs.Msg CustomFormMessage ApiError)
    | SetLevel String
    | SetActiveChapter Chapter
    | ViewSummaryReport
    | ViewTodos
    | PostForSummaryReportCompleted (Result ApiError SummaryReport)
    | CloseFeedback
    | FeedbackFormMsg Form.Msg
    | PostFeedbackCompleted (Result ApiError Feedback)
    | SendFeedbackForm
    | GetFeedbacksCompleted (Result ApiError (List Feedback))
    | GetTypeHintsCompleted (Result ApiError (List TypeHint))
    | ScrollToTodo QuestionnaireTodo


type CustomFormMessage
    = FeedbackMsg
    | AddTodo String
    | RemoveTodo String
