module Common.Questionnaire.Msgs exposing (..)

import Common.Questionnaire.Models exposing (Feedback)
import Form
import FormEngine.Msgs
import Http
import KMEditor.Common.Models.Entities exposing (Chapter)


type Msg
    = FormMsg (FormEngine.Msgs.Msg CustomFormMessage)
    | SetActiveChapter Chapter
    | CloseFeedback
    | FeedbackFormMsg Form.Msg
    | PostFeedbackCompleted (Result Http.Error Feedback)
    | SendFeedbackForm
    | GetFeedbacksCompleted (Result Http.Error (List Feedback))


type CustomFormMessage
    = FeedbackMsg
