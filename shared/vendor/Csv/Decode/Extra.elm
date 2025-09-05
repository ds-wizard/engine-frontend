module Csv.Decode.Extra exposing (email, hardcoded, maybeString, notEmptyString)

import Csv.Decode as Decode exposing (Decoder)
import Internal.Utils.RegexPatterns as RegexPatterns
import Regex
import String.Extra as String


email : Decoder String
email =
    let
        validateEmail value =
            if Regex.contains RegexPatterns.email value then
                Decode.succeed value

            else
                Decode.fail ("Invalid email value \"" ++ value ++ "\"")
    in
    Decode.andThen validateEmail Decode.string


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
