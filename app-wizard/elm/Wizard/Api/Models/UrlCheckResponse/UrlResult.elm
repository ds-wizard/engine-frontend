module Wizard.Api.Models.UrlCheckResponse.UrlResult exposing
    ( UrlResult
    , decoder
    , toReadableErrorString
    )

import Gettext exposing (gettext)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import String.Format as String
import Wizard.Api.Models.UrlCheckResponse.UrlResult.ErrorCode as ErrorCode exposing (ErrorCode)


type alias UrlResult =
    { url : String
    , ok : Bool
    , errorCode : Maybe ErrorCode
    , errorMessage : Maybe String
    , httpStatus : Maybe Int
    , contentType : Maybe String
    , reason : Maybe String
    }


decoder : Decoder UrlResult
decoder =
    D.succeed UrlResult
        |> D.required "url" D.string
        |> D.required "ok" D.bool
        |> D.required "error_code" (D.maybe ErrorCode.decoder)
        |> D.required "error_message" (D.maybe D.string)
        |> D.required "http_status" (D.maybe D.int)
        |> D.required "content_type" (D.maybe D.string)
        |> D.required "reason" (D.maybe D.string)


toReadableErrorString : Gettext.Locale -> UrlResult -> Maybe String
toReadableErrorString locale urlResult =
    if urlResult.ok then
        Nothing

    else
        Just <|
            case urlResult.errorCode of
                Just ErrorCode.Timeout ->
                    gettext "The request timed out." locale

                Just ErrorCode.ConnectTimeout ->
                    gettext "The connection timed out." locale

                Just ErrorCode.InvalidUrl ->
                    gettext "The URL is invalid." locale

                Just ErrorCode.HttpError ->
                    case urlResult.httpStatus of
                        Just status ->
                            String.format
                                (gettext "HTTP error %s occurred." locale)
                                [ String.fromInt status ]

                        Nothing ->
                            gettext "An HTTP error occurred." locale

                Just ErrorCode.NotHtml ->
                    case urlResult.contentType of
                        Just contentType ->
                            String.format
                                (gettext "The content type '%s' is not HTML." locale)
                                [ contentType ]

                        Nothing ->
                            gettext "The content is not HTML." locale

                Just ErrorCode.NetworkError ->
                    gettext "A network error occurred." locale

                Just ErrorCode.UnknownError ->
                    gettext "An unknown error occurred." locale

                _ ->
                    gettext "An unknown error occurred." locale
