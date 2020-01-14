module Registry.Utils exposing (dispatch, validateRegex)

import Form.Error as Error exposing (ErrorValue(..))
import Form.Validate as Validate exposing (Validation, mapError)
import Regex exposing (Regex)
import Task


validateRegex : String -> Validation e String
validateRegex regex =
    Validate.string
        |> Validate.andThen
            (\s -> Validate.format (createRegex regex) s |> mapError (\_ -> Error.value InvalidFormat))


createRegex : String -> Regex
createRegex regex =
    Maybe.withDefault Regex.never <| Regex.fromString regex


dispatch : a -> Cmd a
dispatch msg =
    Task.perform (always msg) (Task.succeed ())
