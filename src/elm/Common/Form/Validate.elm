module Common.Form.Validate exposing (uuidPattern, validateConfirmation, validateRegexWithCustomError, validateUuid)

import Common.Form exposing (CustomFormError(..))
import Form.Validate as Validate exposing (..)
import Regex exposing (Regex)


validateConfirmation : String -> Validation CustomFormError String -> Validation CustomFormError String
validateConfirmation confirmationField =
    let
        validate original =
            Validate.field confirmationField
                (Validate.string
                    |> Validate.andThen
                        (\confirmation ->
                            if original == confirmation then
                                Validate.succeed confirmation
                            else
                                Validate.fail (customError ConfirmationError)
                        )
                )
    in
    Validate.andThen validate


validateRegexWithCustomError : Regex -> CustomFormError -> Validation CustomFormError String
validateRegexWithCustomError regex customFormError =
    Validate.string
        |> Validate.andThen
            (\s ->
                Validate.format regex s
                    |> Validate.mapError (\_ -> customError customFormError)
            )


uuidPattern : Regex
uuidPattern =
    let
        regex = "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
        options = { caseInsensitive = True, multiline = False }
    in
    Maybe.withDefault Regex.never <| Regex.fromStringWith  options regex


validateUuid : Validation CustomFormError String
validateUuid =
    validateRegexWithCustomError uuidPattern InvalidUuid
