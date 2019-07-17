module Common.Api.Feedbacks exposing (getFeedbacks, postFeedback)

import Common.Api exposing (ToMsg, httpFetch, httpGet)
import Common.AppState exposing (AppState)
import Common.Questionnaire.Models.Feedback as Feedback exposing (Feedback)
import Json.Encode exposing (Value)


getFeedbacks : String -> String -> AppState -> ToMsg (List Feedback) msg -> Cmd msg
getFeedbacks packageId questionUuid =
    httpGet ("/feedbacks?packageId=" ++ packageId ++ "&questionUuid=" ++ questionUuid) Feedback.listDecoder


postFeedback : Value -> AppState -> ToMsg Feedback msg -> Cmd msg
postFeedback =
    httpFetch "/feedbacks" Feedback.decoder
