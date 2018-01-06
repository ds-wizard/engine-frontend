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
