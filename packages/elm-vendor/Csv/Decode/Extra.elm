module Csv.Decode.Extra exposing (email, hardcoded, maybeString, notEmptyString)

import Csv.Decode as Decode exposing (Decoder)
import Regex
import String.Extra as String


email : Decoder String
email =
    let
        validateEmail value =
            if Regex.contains emailPattern value then
                Decode.succeed value

            else
                Decode.fail ("Invalid email value \"" ++ value ++ "\"")
    in
    Decode.andThen validateEmail Decode.string


emailPattern : Regex.Regex
emailPattern =
    "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never


notEmptyString : Decoder String
notEmptyString =
    let
        validateNotEmpty value =
            if String.isEmpty value then
                Decode.fail "Field cannot be empty"

            else
                Decode.succeed value
    in
    Decode.andThen validateNotEmpty Decode.string


maybeString : Decoder (Maybe String)
maybeString =
    Decode.andThen (Decode.succeed << String.toMaybe) Decode.string


hardcoded : a -> Decoder (a -> b) -> Decoder b
hardcoded value =
    Decode.andThen (\a -> Decode.succeed (a value))
