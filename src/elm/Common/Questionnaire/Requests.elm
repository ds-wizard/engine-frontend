module Common.Questionnaire.Requests exposing (getFeedbacks, postFeedback, postForSummaryReport)

import Auth.Models exposing (Session)
import Common.Questionnaire.Models exposing (Feedback, feedbackDecoder, feedbackListDecoder)
import Common.Questionnaire.Models.SummaryReport exposing (SummaryReport, summaryReportDecoder)
import Http
import Json.Encode exposing (Value)
import Jwt
import Requests exposing (apiUrl)


postFeedback : Value -> Http.Request Feedback
postFeedback feedback =
    Http.post (apiUrl "/feedbacks") (Http.jsonBody feedback) feedbackDecoder


getFeedbacks : String -> String -> Http.Request (List Feedback)
getFeedbacks packageId questionUuid =
    Http.get (apiUrl <| "/feedbacks?packageId=" ++ packageId ++ "&questionUuid=" ++ questionUuid) feedbackListDecoder


postForSummaryReport : Session -> String -> Value -> Http.Request SummaryReport
postForSummaryReport session questionnaireUuid questionnaire =
    Requests.postWithResponse
        questionnaire
        session
        ("/questionnaires/" ++ questionnaireUuid ++ "/report/preview")
        summaryReportDecoder
