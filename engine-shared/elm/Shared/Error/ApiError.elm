module Shared.Error.ApiError exposing
    ( ApiError(..)
    , toActionResult
    , toServerError
    )

import ActionResult exposing (ActionResult(..))
import Json.Decode exposing (decodeString)
import Shared.Error.ServerError as ServerError exposing (ServerError)


type ApiError
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int String
    | BadBody String


toServerError : ApiError -> Maybe ServerError
toServerError error =
    case error of
        BadStatus 500 _ ->
            Nothing

        BadStatus _ response ->
            case decodeString ServerError.decoder response of
                Ok err ->
                    Just err

                _ ->
                    Nothing

        _ ->
            Nothing


toActionResult : String -> ApiError -> ActionResult a
toActionResult defaultMessage error =
    case toServerError error of
        Just err ->
            if String.isEmpty err.message then
                Error defaultMessage

            else
                Error err.message

        Nothing ->
            Error defaultMessage
