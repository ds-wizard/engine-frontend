module Common.Api.Feedbacks exposing (getFeedbacks, postFeedback)

import Common.Api exposing (ToMsg, httpFetch, httpGet)
import Common.AppState exposing (AppState)
import Common.Questionnaire.Models exposing (Feedback, feedbackDecoder, feedbackListDecoder)
import Json.Encode exposing (Value)


getFeedbacks : String -> String -> AppState -> ToMsg (List Feedback) msg -> Cmd msg
getFeedbacks packageId questionUuid =
    httpGet ("/feedbacks?packageId=" ++ packageId ++ "&questionUuid=" ++ questionUuid) feedbackListDecoder


postFeedback : Value -> AppState -> ToMsg Feedback msg -> Cmd msg
postFeedback =
    httpFetch "/feedbacks" feedbackDecoder
