port module Common.Ports.Tour exposing (drive, onDone)

{-| driver.js, for guided tours. See `packages/elm-common/js/ports/tour.js`, and
`Common.Components.Tour` for the Elm side anyone should call.
-}

import Json.Encode as E


drive : E.Value -> Cmd msg
drive =
    commonDriveTour


{-| The tour id, once a tour has ended — finished or skipped.
-}
onDone : (String -> msg) -> Sub msg
onDone =
    commonOnTourDone


port commonDriveTour : E.Value -> Cmd msg


port commonOnTourDone : (String -> msg) -> Sub msg
