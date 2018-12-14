module Common.Models exposing (ServerError, decodeError, errorDecoder, fieldErrorDecoder, getServerError, getServerErrorJwt)

import ActionResult exposing (ActionResult(..))
import Http exposing (Error(..), Response)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (optional, required)
import Jwt


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


decodeError : Http.Error -> Maybe ServerError
decodeError error =
    case error of
        BadStatus response ->
            case decodeString errorDecoder response.body of
                Ok err ->
                    Just err

                _ ->
                    Nothing

        _ ->
            Nothing


getServerError : Http.Error -> String -> ActionResult a
getServerError error defaultMessage =
    case decodeError error of
        Just err ->
            if String.isEmpty err.message then
                Error defaultMessage

            else
                Error err.message

        Nothing ->
            Error defaultMessage


getServerErrorJwt : Jwt.JwtError -> String -> ActionResult a
getServerErrorJwt error defaultMessage =
    case error of
        Jwt.HttpError httpError ->
            getServerError httpError defaultMessage

        _ ->
            Error defaultMessage
