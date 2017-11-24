module Utils exposing (..)

import Form.Error as Error exposing (Error, ErrorValue(..))
import Form.Validate as Validate exposing (..)
import Random.Pcg exposing (Seed, step)
import Regex exposing (Regex)
import Uuid


tuplePrepend : a -> ( b, c ) -> ( a, b, c )
tuplePrepend a ( b, c ) =
    ( a, b, c )


validateRegex : String -> Validation e String
validateRegex regex =
    Validate.string
        |> Validate.andThen
            (\s -> Validate.format (Regex.regex regex) s |> mapError (\_ -> Error.value InvalidFormat))


getUuid : Seed -> ( String, Seed )
getUuid seed =
    let
        ( uuid, newSeed ) =
            step Uuid.uuidGenerator seed
    in
    ( Uuid.toString uuid, newSeed )
