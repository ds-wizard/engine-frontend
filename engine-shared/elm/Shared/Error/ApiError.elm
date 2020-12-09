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
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int String
    | BadBody String


toServerError : ApiError -> Maybe ServerError
toServerError error =
    case error of
        BadStatus 403 _ ->
            Just ServerError.ForbiddenError

        BadStatus 500 _ ->
            Nothing

        BadStatus _ response ->
            case decodeString ServerError.decoder response of
                Ok err ->
                    Just err

                Err _ ->
                    Nothing

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
