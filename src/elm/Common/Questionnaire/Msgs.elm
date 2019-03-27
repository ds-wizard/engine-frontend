module Common.Questionnaire.Msgs exposing (CustomFormMessage(..), Msg(..))

import Common.ApiError exposing (ApiError)
import Common.Questionnaire.Models exposing (Feedback)
import Common.Questionnaire.Models.SummaryReport exposing (SummaryReport)
import Form
import FormEngine.Msgs
import KMEditor.Common.Models.Entities exposing (Chapter, Metric)


type Msg
    = FormMsg (FormEngine.Msgs.Msg CustomFormMessage)
    | SetLevel String
    | SetActiveChapter Chapter
    | ViewSummaryReport
    | PostForSummaryReportCompleted (Result ApiError SummaryReport)
    | CloseFeedback
    | FeedbackFormMsg Form.Msg
    | PostFeedbackCompleted (Result ApiError Feedback)
    | SendFeedbackForm
    | GetFeedbacksCompleted (Result ApiError (List Feedback))


type CustomFormMessage
    = FeedbackMsg
