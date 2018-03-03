module Common.Form.Validate exposing (..)

import Form.Validate as Validate exposing (..)


type CustomFormError
    = ConfirmationError


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
