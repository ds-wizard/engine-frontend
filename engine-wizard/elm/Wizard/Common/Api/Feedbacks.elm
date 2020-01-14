module Wizard.Common.Api.Feedbacks exposing (getFeedbacks, postFeedback)

import Json.Encode exposing (Value)
import Wizard.Common.Api exposing (ToMsg, httpFetch, httpGet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Questionnaire.Models.Feedback as Feedback exposing (Feedback)


getFeedbacks : String -> String -> AppState -> ToMsg (List Feedback) msg -> Cmd msg
getFeedbacks packageId questionUuid =
    httpGet ("/feedbacks?packageId=" ++ packageId ++ "&questionUuid=" ++ questionUuid) Feedback.listDecoder


postFeedback : Value -> AppState -> ToMsg Feedback msg -> Cmd msg
postFeedback =
    httpFetch "/feedbacks" Feedback.decoder
