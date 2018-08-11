module Common.Types exposing (..)

{-|

@docs ActionResult

-}


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


withDefault : a -> ActionResult a -> a
withDefault default result =
    case result of
        Success a ->
            a

        _ ->
            default


combine : ActionResult a -> ActionResult b -> ActionResult ( a, b )
combine actionResult1 actionResult2 =
    case ( actionResult1, actionResult2 ) of
        ( Success a, Success b ) ->
            Success ( a, b )

        ( Unset, _ ) ->
            Unset

        ( _, Unset ) ->
            Unset

        ( Loading, _ ) ->
            Loading

        ( _, Loading ) ->
            Loading

        ( Error a, _ ) ->
            Error a

        ( _, Error b ) ->
            Error b


combine3 : ActionResult a -> ActionResult b -> ActionResult c -> ActionResult ( a, b, c )
combine3 actionResult1 actionResult2 actionResult3 =
    case combine (combine actionResult1 actionResult2) actionResult3 of
        Unset ->
            Unset

        Loading ->
            Loading

        Error e ->
            Error e

        Success ( ( a, b ), c ) ->
            Success ( a, b, c )
