module Common.Form exposing
    ( CustomFormError(..)
    , setFormErrors
    )

import Common.ApiError exposing (ApiError, decodeApiError)
import Form exposing (Form)
import Form.Validate as Validate exposing (..)


type CustomFormError
    = ConfirmationError
    | InvalidUuid
    | ServerValidationError String
    | Error String


setFormErrors : ApiError -> Form CustomFormError a -> Form CustomFormError a
setFormErrors apiError form =
    case decodeApiError apiError of
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
