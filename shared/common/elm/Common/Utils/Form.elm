module Common.Utils.Form exposing
    ( containsChanges
    , errorToString
    , setFormErrors
    )

import Common.Data.ApiError as ApiError exposing (ApiError)
import Common.Data.ServerError as ServerError
import Common.Utils.Form.FormError exposing (FormError(..))
import Dict
import Form exposing (Form)
import Form.Error exposing (ErrorValue(..))
import Form.Validate as V exposing (Validation, customError)
import Gettext exposing (gettext)
import Set
import String.Format as String


errorToString : Gettext.Locale -> String -> ErrorValue FormError -> String
errorToString locale labelText error =
    case error of
        Empty ->
            String.format (gettext "%s cannot be empty." locale) [ labelText ]

        InvalidString ->
            String.format (gettext "%s cannot be empty." locale) [ labelText ]

        InvalidEmail ->
            gettext "This is not a valid email." locale

        InvalidFloat ->
            gettext "This is not a valid number." locale

        SmallerFloatThan n ->
            String.format (gettext "This should not be less than %s." locale) [ String.fromFloat n ]

        GreaterFloatThan n ->
            String.format (gettext "This should not be more than %s." locale) [ String.fromFloat n ]

        CustomError err ->
            case err of
                ConfirmationError ->
                    gettext "Passwords do not match!" locale

                InvalidUuid ->
                    gettext "This is not a valid UUID." locale

                ServerValidationError msg ->
                    msg

                Error msg ->
                    msg

        _ ->
            gettext "Invalid value." locale


setFormErrors : { b | locale : Gettext.Locale } -> ApiError -> Form FormError a -> Form FormError a
setFormErrors appState apiError form =
    case ApiError.toServerError apiError of
        Just (ServerError.UserFormError error) ->
            List.foldl (setFormError appState) form <| Dict.toList error.fieldErrors

        _ ->
            form


setFormError : { b | locale : Gettext.Locale } -> ( String, List ServerError.Message ) -> Form FormError a -> Form FormError a
setFormError appState ( fieldName, fieldErrors ) form =
    case List.head fieldErrors of
        Just fieldError ->
            Form.update (createFieldValidation appState fieldName fieldError) Form.Validate form

        _ ->
            form


createFieldValidation : { b | locale : Gettext.Locale } -> String -> ServerError.Message -> Validation FormError a
createFieldValidation appState fieldName fieldError =
    let
        error =
            Maybe.withDefault "" <|
                ServerError.messageToReadable appState fieldError
    in
    V.field fieldName (V.fail (customError (ServerValidationError error)))


containsChanges : Form e a -> Bool
containsChanges =
    let
        isNotHelperField =
            not << String.endsWith "__"
    in
    not << Set.isEmpty << Set.filter isNotHelperField << Form.getChangedFields
