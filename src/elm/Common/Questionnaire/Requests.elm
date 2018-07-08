module Common.Questionnaire.Requests exposing (..)

import Common.Questionnaire.Models exposing (Feedback, feedbackDecoder, feedbackListDecoder)
import Http
import Json.Encode exposing (Value)
import Requests exposing (apiUrl)


postFeedback : Value -> Http.Request Feedback
postFeedback feedback =
    Http.post (apiUrl "/feedbacks") (Http.jsonBody feedback) feedbackDecoder


getFeedbacks : String -> String -> Http.Request (List Feedback)
getFeedbacks packageId questionUuid =
    Http.get (apiUrl <| "/feedbacks?packageId=" ++ packageId ++ "&questionUuid=" ++ questionUuid) feedbackListDecoder
