module Auth.Requests exposing (..)

import Auth.Models exposing (Session)
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Jwt
import Requests exposing (apiUrl)
import UserManagement.Models exposing (User, userDecoder)


authUser : { r | email : String, password : String } -> Http.Request String
authUser { email, password } =
    let
        body =
            encodeCredentials email password |> Http.jsonBody
    in
    Http.post (apiUrl "/tokens") body tokenDecoder


encodeCredentials : String -> String -> Encode.Value
encodeCredentials email password =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]


tokenDecoder : Decoder String
tokenDecoder =
    Decode.field "token" Decode.string


getCurrentUser : Session -> Http.Request User
getCurrentUser session =
    Jwt.get session.token (apiUrl "/users/current") userDecoder
