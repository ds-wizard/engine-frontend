module Common.Types exposing (..)


type ActionResult a
    = Unset
    | Loading
    | Success a
    | Error String
