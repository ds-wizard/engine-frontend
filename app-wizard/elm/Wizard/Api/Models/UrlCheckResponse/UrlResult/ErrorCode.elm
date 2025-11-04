module Wizard.Api.Models.UrlCheckResponse.UrlResult.ErrorCode exposing
    ( ErrorCode(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type ErrorCode
    = Timeout
    | ConnectTimeout
    | InvalidUrl
    | HttpError
    | NotHtml
    | NetworkError
    | UnknownError


decoder : Decoder ErrorCode
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "TIMEOUT" ->
                        D.succeed Timeout

                    "CONNECT_TIMEOUT" ->
                        D.succeed ConnectTimeout

                    "HTTP_ERROR" ->
                        D.succeed HttpError

                    "INVALID_URL" ->
                        D.succeed InvalidUrl

                    "NOT_HTML" ->
                        D.succeed NotHtml

                    "NETWORK_ERROR" ->
                        D.succeed NetworkError

                    "UNKNOWN_ERROR" ->
                        D.succeed UnknownError

                    _ ->
                        D.fail ("Unknown ErrorCode: " ++ str)
            )
