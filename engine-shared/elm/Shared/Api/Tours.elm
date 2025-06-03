module Shared.Api.Tours exposing
    ( putTour
    , resetTours
    )

import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtDelete, jwtPutEmpty)


putTour : String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putTour tourId =
    jwtPutEmpty ("/users/current/tours/" ++ tourId)


resetTours : AbstractAppState a -> ToMsg () msg -> Cmd msg
resetTours =
    jwtDelete "/users/current/tours"
