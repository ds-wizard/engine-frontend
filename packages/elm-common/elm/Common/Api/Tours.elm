module Common.Api.Tours exposing
    ( putTour
    , resetTours
    )

import Common.Api.Context as Context exposing (Context)
import Common.Api.Request as Request exposing (ToMsg)


putTour : Context ctx -> String -> ToMsg () msg -> Cmd msg
putTour appState tourId =
    Request.putEmpty (Context.toServerInfo appState) ("/users/current/tours/" ++ tourId)


resetTours : Context ctx -> ToMsg () msg -> Cmd msg
resetTours appState =
    Request.delete (Context.toServerInfo appState) "/users/current/tours"
