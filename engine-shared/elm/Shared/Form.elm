module Shared.Form exposing
    ( errorToString
    , setFormErrors
    )

import Dict
import Form exposing (Form)
import Form.Error exposing (ErrorValue(..))
import Form.Validate as V exposing (Validation, customError)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Error.ServerError as ServerError
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


setFormErrors : { b | provisioning : Provisioning } -> ApiError -> Form FormError a -> Form FormError a
setFormErrors appState apiError form =
    case ApiError.toServerError apiError of
        Just (ServerError.UserFormError error) ->
            List.foldl (setFormError appState) form <| Dict.toList error.fieldErrors

        _ ->
            form


setFormError : { b | provisioning : Provisioning } -> ( String, List ServerError.Message ) -> Form FormError a -> Form FormError a
setFormError appState ( fieldName, fieldErrors ) form =
    case List.head fieldErrors of
        Just fieldError ->
            Form.update (createFieldValidation appState fieldName fieldError) Form.Validate form

        _ ->
            form


createFieldValidation : { b | provisioning : Provisioning } -> String -> ServerError.Message -> Validation FormError a
createFieldValidation appState fieldName fieldError =
    let
        error =
            Maybe.withDefault "" <|
                ServerError.messageToReadable appState fieldError
    in
    V.field fieldName (V.fail (customError (ServerValidationError error)))
