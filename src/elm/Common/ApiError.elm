module Common.ApiError exposing
    ( ApiError(..)
    , ServerError
    , decodeApiError
    , errorDecoder
    , fieldErrorDecoder
    , getServerError
    )

import ActionResult exposing (ActionResult(..))
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (optional, required)


type ApiError
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int String
    | BadBody String


type alias ServerError =
    { message : String
    , fieldErrors : List ( String, String )
    }


errorDecoder : Decoder ServerError
errorDecoder =
    Decode.succeed ServerError
        |> required "message" Decode.string
        |> optional "fieldErrors" (Decode.list <| fieldErrorDecoder) []


fieldErrorDecoder : Decoder ( String, String )
fieldErrorDecoder =
    Decode.map2 (\a b -> ( a, b )) (index 0 Decode.string) (index 1 Decode.string)


decodeApiError : ApiError -> Maybe ServerError
decodeApiError error =
    case error of
        BadStatus _ response ->
            case decodeString errorDecoder response of
                Ok err ->
                    Just err

                _ ->
                    Nothing

        _ ->
            Nothing


getServerError : ApiError -> String -> ActionResult a
getServerError error defaultMessage =
    case decodeApiError error of
        Just err ->
            if String.isEmpty err.message then
                Error defaultMessage

            else
                Error err.message

        Nothing ->
            Error defaultMessage
