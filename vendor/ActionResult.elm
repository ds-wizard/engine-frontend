module ActionResult exposing
    ( ActionResult(..)
    , apply
    , combine
    , combine3
    , isError
    , isLoading
    , isSuccess
    , isUnset
    , map
    , withDefault
    )


type ActionResult a
    = Unset
    | Loading
    | Success a
    | Error String


isUnset : ActionResult a -> Bool
isUnset actionResult =
    case actionResult of
        Unset ->
            True

        _ ->
            False


isLoading : ActionResult a -> Bool
isLoading actionResult =
    case actionResult of
        Loading ->
            True

        _ ->
            False


isSuccess : ActionResult a -> Bool
isSuccess actionResult =
    case actionResult of
        Success _ ->
            True

        _ ->
            False


isError : ActionResult a -> Bool
isError actionResult =
    case actionResult of
        Error _ ->
            True

        _ ->
            False


map : (a -> b) -> ActionResult a -> ActionResult b
map fn actionResult =
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


apply :
    (ActionResult data -> model -> model)
    -> (e -> ActionResult data)
    -> Result e data
    -> model
    -> model
apply setData convertError result =
    case result of
        Ok data ->
            setData (Success data)

        Err err ->
            setData (convertError err)
