module Common.Utils.Form.FormError exposing (FormError(..))


type FormError
    = ConfirmationError
    | InvalidUuid
    | InvalidColor
    | PasswordTooWeak
    | ServerValidationError String
    | Error String
