module Common.Form.Validate exposing (..)

import Common.Form exposing (CustomFormError(ConfirmationError, InvalidUuid))
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
    Regex.regex "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
        |> Regex.caseInsensitive


validateUuid : Validation CustomFormError String
validateUuid =
    validateRegexWithCustomError uuidPattern InvalidUuid
