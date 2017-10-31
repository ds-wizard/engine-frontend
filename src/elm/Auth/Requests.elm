module Auth.Requests exposing (..)

import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Requests exposing (apiUrl)


authUser : { r | email : String, password : String } -> Http.Request String
authUser { email, password } =
    let
        body =
            encodeCredentials email password |> Http.jsonBody
    in
    Http.post (apiUrl "/token") body tokenDecoder


encodeCredentials : String -> String -> Encode.Value
encodeCredentials email password =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]


tokenDecoder : Decoder String
tokenDecoder =
    Decode.field "token" Decode.string
