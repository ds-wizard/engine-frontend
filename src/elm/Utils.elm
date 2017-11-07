module Utils exposing (..)


type FormResult
    = Success String
    | Error String
    | None


tuplePrepend : a -> ( b, c ) -> ( a, b, c )
tuplePrepend a ( b, c ) =
    ( a, b, c )
