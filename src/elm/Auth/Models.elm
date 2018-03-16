module Auth.Models exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Jwt exposing (decodeToken)
import UserManagement.Common.Models exposing (User, userDecoder)


type alias Session =
    { token : String
    , user : Maybe User
    }


type alias JwtToken =
    { permissions : List String }



-- Session helpers


initialSession : Session
initialSession =
    { token = ""
    , user = Nothing
    }


setToken : Session -> String -> Session
setToken session token =
    { session | token = token }


setUser : Session -> User -> Session
setUser session user =
    { session | user = Just user }


sessionDecoder : Decoder Session
sessionDecoder =
    decode Session
        |> required "token" Decode.string
        |> required "user" (Decode.nullable userDecoder)


sessionExists : Session -> Bool
sessionExists session =
    session.token /= ""



-- JWT helpers


parseJwt : String -> Maybe JwtToken
parseJwt token =
    case decodeToken jwtDecoder token of
        Ok jwt ->
            Just jwt

        Err error ->
            Nothing


jwtDecoder : Decoder JwtToken
jwtDecoder =
    decode JwtToken
        |> required "permissions" (Decode.list Decode.string)
