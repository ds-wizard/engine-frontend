module Wizard.Api.Feedbacks exposing (getFeedbacks, postFeedback)

import Json.Decode as D
import Json.Encode exposing (Value)
import Shared.Api.Request as Request exposing (ToMsg)
import Wizard.Api.Models.Feedback as Feedback exposing (Feedback)
import Wizard.Data.AppState as AppState exposing (AppState)


getFeedbacks : AppState -> String -> String -> ToMsg (List Feedback) msg -> Cmd msg
getFeedbacks appState packageId questionUuid =
    let
        url =
            "/feedbacks?packageId=" ++ packageId ++ "&questionUuid=" ++ questionUuid
    in
    Request.get (AppState.toServerInfo appState) url (D.list Feedback.decoder)


postFeedback : AppState -> Value -> ToMsg Feedback msg -> Cmd msg
postFeedback appState body =
    Request.post (AppState.toServerInfo appState) "/feedbacks" Feedback.decoder body
