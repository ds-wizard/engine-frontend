module Common.Utils.Form exposing
    ( containsChanges
    , errorToString
    , fieldErrorToString
    , isValid
    , moveListItem
    , reset
    , setFormErrors
    )

import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.ServerError as ServerError
import Common.Utils.Form.FormError exposing (FormError(..))
import Dict
import Form exposing (Form)
import Form.Error exposing (ErrorValue(..))
import Form.Field as Field
import Form.Validate as V exposing (Validation, customError)
import Gettext exposing (gettext)
import Maybe.Extra as Maybe
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

        InvalidInt ->
            gettext "This is not a valid number." locale

        SmallerFloatThan n ->
            String.format (gettext "This should not be less than %s." locale) [ String.fromFloat n ]

        GreaterFloatThan n ->
            String.format (gettext "This should not be more than %s." locale) [ String.fromFloat n ]

        SmallerIntThan n ->
            String.format (gettext "This should not be less than %s." locale) [ String.fromInt n ]

        GreaterIntThan n ->
            String.format (gettext "This should not be more than %s." locale) [ String.fromInt n ]

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


{-| Message for a field's live error, if it has one.

`errorToString` alone cannot tell an empty field from one holding something
invalid - elm-form reports `InvalidInt` for both an empty and a non-numeric
number field - so the empty case is decided from the raw value instead.

-}
fieldErrorToString : Gettext.Locale -> String -> Form.FieldState FormError String -> Maybe String
fieldErrorToString locale labelText field =
    let
        toString error =
            if Maybe.unwrap True String.isEmpty field.value then
                String.format (gettext "%s cannot be empty." locale) [ labelText ]

            else
                errorToString locale labelText error
    in
    Maybe.map toString field.liveError


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


isValid : Form e a -> Bool
isValid =
    List.isEmpty << Form.getErrors


reset : (a -> Form e a) -> Form e a -> Form e a
reset initForm form =
    Maybe.unwrap form initForm (Form.getOutput form)


{-| Move an item in a form list field by swapping the values of all its fields
with the item at the target index. The `enbala/elm-form` library has no move
message, so the reorder is done by swapping each field value (which correctly
marks the form as changed, unlike `Form.Reset`). Each field is swapped as a
boolean when it holds a boolean value, otherwise as a string.
-}
moveListItem : Validation FormError a -> String -> List String -> Int -> Int -> Form FormError a -> Form FormError a
moveListItem validation listName fields fromIndex toIndex form =
    List.foldl (swapListItemField validation listName fromIndex toIndex form) form fields


swapListItemField : Validation FormError a -> String -> Int -> Int -> Form FormError a -> String -> Form FormError a -> Form FormError a
swapListItemField validation listName fromIndex toIndex originalForm fieldName accForm =
    let
        pathAt index =
            listName ++ "." ++ String.fromInt index ++ "." ++ fieldName

        pathFrom =
            pathAt fromIndex

        pathTo =
            pathAt toIndex

        isBool =
            (Form.getFieldAsBool pathFrom originalForm).value
                /= Nothing
                || (Form.getFieldAsBool pathTo originalForm).value
                /= Nothing
    in
    if isBool then
        let
            fromValue =
                Maybe.withDefault False (Form.getFieldAsBool pathFrom originalForm).value

            toValue =
                Maybe.withDefault False (Form.getFieldAsBool pathTo originalForm).value
        in
        accForm
            |> Form.update validation (Form.Input pathFrom Form.Checkbox (Field.Bool toValue))
            |> Form.update validation (Form.Input pathTo Form.Checkbox (Field.Bool fromValue))

    else
        let
            fromValue =
                Maybe.withDefault "" (Form.getFieldAsString pathFrom originalForm).value

            toValue =
                Maybe.withDefault "" (Form.getFieldAsString pathTo originalForm).value
        in
        accForm
            |> Form.update validation (Form.Input pathFrom Form.Text (Field.String toValue))
            |> Form.update validation (Form.Input pathTo Form.Text (Field.String fromValue))
