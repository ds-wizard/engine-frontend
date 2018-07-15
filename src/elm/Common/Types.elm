module Common.Types exposing (..)

{-|

@docs ActionResult

-}


{-| -}
type ActionResult a
    = Unset
    | Loading
    | Success a
    | Error String


mapSuccess : (a -> b) -> ActionResult a -> ActionResult b
mapSuccess fn actionResult =
    case actionResult of
        Success value ->
            Success <| fn value

        Unset ->
            Unset

        Loading ->
            Loading

        Error err ->
            Error err
