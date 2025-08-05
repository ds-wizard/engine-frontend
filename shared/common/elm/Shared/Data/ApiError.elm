module Shared.Data.ApiError exposing
    ( ApiError(..)
    , toActionResult
    , toServerError
    )

import ActionResult exposing (ActionResult(..))
import Gettext
import Json.Decode exposing (decodeString)
import Shared.Data.ServerError as ServerError exposing (ServerError)


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



-- TODO improve API to not use appState like thing


toActionResult : { b | locale : Gettext.Locale } -> String -> ApiError -> ActionResult a
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
