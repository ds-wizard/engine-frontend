module Shared.Form exposing (..)

import Form exposing (Form)
import Form.Error exposing (ErrorValue(..))
import Form.Validate as V exposing (Validation, customError)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError(..))
import Shared.Locale exposing (l, lf)
import Shared.Provisioning exposing (Provisioning)


l_ : String -> { a | provisioning : Provisioning } -> String
l_ =
    l "Shared.Form"


lf_ : String -> List String -> { a | provisioning : Provisioning } -> String
lf_ =
    lf "Shared.Form"


errorToString : { a | provisioning : Provisioning } -> String -> ErrorValue FormError -> String
errorToString appState labelText error =
    case error of
        Empty ->
            lf_ "error.empty" [ labelText ] appState

        InvalidString ->
            lf_ "error.invalidString" [ labelText ] appState

        InvalidEmail ->
            l_ "error.invalidEmail" appState

        InvalidFloat ->
            l_ "error.invalidFloat" appState

        SmallerFloatThan n ->
            lf_ "error.smallerFloatThan" [ String.fromFloat n ] appState

        GreaterFloatThan n ->
            lf_ "error.greaterFloatThan" [ String.fromFloat n ] appState

        CustomError err ->
            case err of
                ConfirmationError ->
                    l_ "error.confirmationError" appState

                InvalidUuid ->
                    l_ "error.invalidUuid" appState

                ServerValidationError msg ->
                    msg

                Error msg ->
                    msg

                IntegrationIdAlreadyUsed ->
                    l_ "error.integrationIdAlreadyUsed" appState

        _ ->
            l_ "error.default" appState


setFormErrors : ApiError -> Form FormError a -> Form FormError a
setFormErrors apiError form =
    case ApiError.toServerError apiError of
        Just error ->
            List.foldl setFormError form error.fieldErrors

        _ ->
            form


setFormError : ( String, String ) -> Form FormError a -> Form FormError a
setFormError fieldError form =
    Form.update (createFieldValidation fieldError) Form.Validate form


createFieldValidation : ( String, String ) -> Validation FormError a
createFieldValidation ( fieldName, fieldError ) =
    V.field fieldName (V.fail (customError (ServerValidationError fieldError)))
