module Wizard.Api.Tours exposing
    ( putTour
    , resetTours
    )

import Common.Api.Request as Request exposing (ToMsg)
import Wizard.Data.AppState as AppState exposing (AppState)


putTour : AppState -> String -> ToMsg () msg -> Cmd msg
putTour appState tourId =
    Request.putEmpty (AppState.toServerInfo appState) ("/users/current/tours/" ++ tourId)


resetTours : AppState -> ToMsg () msg -> Cmd msg
resetTours appState =
    Request.delete (AppState.toServerInfo appState) "/users/current/tours"
