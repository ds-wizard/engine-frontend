module Common.Utils.Form.FormError exposing (FormError(..))


type FormError
    = ConfirmationError
    | InvalidUuid
    | PasswordTooWeak
    | ServerValidationError String
    | Error String
