module Common.Models exposing (..)

import Common.Types exposing (ActionResult(Error))
import Http exposing (Error(BadStatus), Response)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Jwt


type alias ServerError =
    { message : String
    , fieldErrors : List ( String, String )
    }


errorDecoder : Decoder ServerError
errorDecoder =
    decode ServerError
        |> required "message" Decode.string
        |> required "fieldErrors" (Decode.list <| fieldErrorDecoder)


fieldErrorDecoder : Decoder ( String, String )
fieldErrorDecoder =
    Decode.map2 (,) (index 0 Decode.string) (index 1 Decode.string)


decodeError : Http.Error -> Maybe ServerError
decodeError error =
    case error of
        BadStatus response ->
            case decodeString errorDecoder response.body of
                Ok error ->
                    Just error

                _ ->
                    Nothing

        _ ->
            Nothing


getServerError : Http.Error -> String -> ActionResult a
getServerError error defaultMessage =
    case decodeError error of
        Just error ->
            if String.isEmpty error.message then
                Error defaultMessage
            else
                Error error.message

        Nothing ->
            Error defaultMessage


getServerErrorJwt : Jwt.JwtError -> String -> ActionResult a
getServerErrorJwt error defaultMessage =
    case error of
        Jwt.HttpError httpError ->
            getServerError httpError defaultMessage

        _ ->
            Error defaultMessage
