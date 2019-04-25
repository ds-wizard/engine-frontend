module Common.Questionnaire.Msgs exposing (CustomFormMessage(..), Msg(..))

import Common.ApiError exposing (ApiError)
import Common.Questionnaire.Models exposing (Feedback)
import Common.Questionnaire.Models.SummaryReport exposing (SummaryReport)
import Form
import FormEngine.Model exposing (TypeHint)
import FormEngine.Msgs
import KMEditor.Common.Models.Entities exposing (Chapter, Metric)
import Result exposing (Result)


type Msg
    = FormMsg (FormEngine.Msgs.Msg CustomFormMessage ApiError)
    | SetLevel String
    | SetActiveChapter Chapter
    | ViewSummaryReport
    | PostForSummaryReportCompleted (Result ApiError SummaryReport)
    | CloseFeedback
    | FeedbackFormMsg Form.Msg
    | PostFeedbackCompleted (Result ApiError Feedback)
    | SendFeedbackForm
    | GetFeedbacksCompleted (Result ApiError (List Feedback))
    | GetTypeHintsCompleted (Result ApiError (List TypeHint))


type CustomFormMessage
    = FeedbackMsg
