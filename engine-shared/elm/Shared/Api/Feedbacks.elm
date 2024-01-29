module Shared.Api.Feedbacks exposing (getFeedbacks, postFeedback)

import Json.Decode as D
import Json.Encode exposing (Value)
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, httpGet, jwtOrHttpFetch)
import Shared.Data.Feedback as Feedback exposing (Feedback)


getFeedbacks : String -> String -> AbstractAppState a -> ToMsg (List Feedback) msg -> Cmd msg
getFeedbacks packageId questionUuid =
    httpGet ("/feedbacks?packageId=" ++ packageId ++ "&questionUuid=" ++ questionUuid) (D.list Feedback.decoder)


postFeedback : Value -> AbstractAppState a -> ToMsg Feedback msg -> Cmd msg
postFeedback =
    jwtOrHttpFetch "/feedbacks" Feedback.decoder
