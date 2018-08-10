module Common.Questionnaire.Msgs exposing (..)

import Common.Questionnaire.Models exposing (Feedback)
import Common.Questionnaire.Models.SummaryReport exposing (SummaryReport)
import Form
import FormEngine.Msgs
import Http
import Jwt
import KMEditor.Common.Models.Entities exposing (Chapter, Metric)


type Msg
    = FormMsg (FormEngine.Msgs.Msg CustomFormMessage)
    | SetActiveChapter Chapter
    | ViewSummaryReport
    | GetMetricsCompleted (Result Jwt.JwtError (List Metric))
    | PostForSummaryReportCompleted (Result Jwt.JwtError SummaryReport)
    | CloseFeedback
    | FeedbackFormMsg Form.Msg
    | PostFeedbackCompleted (Result Http.Error Feedback)
    | SendFeedbackForm
    | GetFeedbacksCompleted (Result Http.Error (List Feedback))


type CustomFormMessage
    = FeedbackMsg
