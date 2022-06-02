module Shared.Error.ApiError exposing
    ( ApiError(..)
    , toActionResult
    , toServerError
    )

import ActionResult exposing (ActionResult(..))
import Json.Decode exposing (decodeString)
import Shared.Error.ServerError as ServerError exposing (ServerError)
import Shared.Provisioning exposing (Provisioning)


type ApiError
    = Timeout
    | NetworkError
    | BadStatus Int String
    | OtherError


toServerError : ApiError -> Maybe ServerError
toServerError error =
    case error of
        BadStatus 403 _ ->
            Just ServerError.ForbiddenError

        BadStatus 500 _ ->
            Nothing

        BadStatus _ response ->
            Result.toMaybe <|
                decodeString ServerError.decoder response

        _ ->
            Nothing


toActionResult : { b | provisioning : Provisioning } -> String -> ApiError -> ActionResult a
toActionResult appState defaultMessage error =
    case toServerError error of
        Just err ->
            case err of
                ServerError.UserSimpleError message ->
                    Error <|
                        Maybe.withDefault defaultMessage <|
                            ServerError.messageToReadable appState message

                ServerError.ForbiddenError ->
                    Error (ServerError.forbiddenMessage appState)

                _ ->
                    Error defaultMessage

        Nothing ->
            Error defaultMessage
