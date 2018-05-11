module Common.Form exposing (..)

import Common.Models exposing (decodeError)
import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Http exposing (Error(BadStatus), Response)


type CustomFormError
    = ConfirmationError
    | ServerValidationError String


setFormErrors : Http.Error -> Form CustomFormError a -> Form CustomFormError a
setFormErrors rawError form =
    case decodeError rawError of
        Just error ->
            List.foldl setFormError form error.fieldErrors

        _ ->
            form


setFormError : ( String, String ) -> Form CustomFormError a -> Form CustomFormError a
setFormError fieldError form =
    Form.update (createFieldValidation fieldError) Form.Validate form


createFieldValidation : ( String, String ) -> Validation CustomFormError a
createFieldValidation ( fieldName, fieldError ) =
    Validate.field fieldName (Validate.fail (customError (ServerValidationError fieldError)))
