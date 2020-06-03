module Shared.Form.FormError exposing (FormError(..))


type FormError
    = ConfirmationError
    | InvalidUuid
    | ServerValidationError String
    | Error String
    | IntegrationIdAlreadyUsed
