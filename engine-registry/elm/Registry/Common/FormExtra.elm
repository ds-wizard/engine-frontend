module Registry.Common.FormExtra exposing (CustomFormError(..), createFieldValidation, setFormError, setFormErrors)

import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Shared.Error.ApiError as ApiError exposing (ApiError)


type CustomFormError
    = ServerValidationError String
    | Error String


setFormErrors : ApiError -> Form CustomFormError a -> Form CustomFormError a
setFormErrors apiError form =
    case ApiError.toServerError apiError of
        Just error ->
            List.foldl setFormError form error.fieldErrors

        _ ->
            form


setFormError : ( String, String ) -> Form CustomFormError a -> Form CustomFormError a
setFormError fieldError form =
    Form.update (createFieldValidation fieldError) Form.Validate form


createFieldValidation : ( String, String ) -> Validation CustomFormError a
createFieldValidation ( fieldName, fieldError ) =
    Validate.field fieldName (Validate.fail (Validate.customError (ServerValidationError fieldError)))
