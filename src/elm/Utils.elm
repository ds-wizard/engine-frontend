module Utils exposing (..)

import Form.Error as Error exposing (Error, ErrorValue(..))
import Form.Validate as Validate exposing (..)
import Regex exposing (Regex)


type FormResult
    = Success String
    | Error String
    | None


tuplePrepend : a -> ( b, c ) -> ( a, b, c )
tuplePrepend a ( b, c ) =
    ( a, b, c )


validateRegex : Regex -> Validation e String
validateRegex regex =
    Validate.string
        |> Validate.andThen
            (\s -> Validate.format regex s |> mapError (\_ -> Error.value InvalidFormat))
