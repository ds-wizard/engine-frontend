module Common.Api.Tokens exposing (fetchToken)

import Common.Api exposing (ToMsg, httpFetch)
import Common.AppState exposing (AppState)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


fetchToken : { r | email : String, password : String } -> AppState -> ToMsg String msg -> Cmd msg
fetchToken { email, password } =
    let
        body =
            encodeCredentials email password
    in
    httpFetch "/tokens" tokenDecoder body


encodeCredentials : String -> String -> Encode.Value
encodeCredentials email password =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]


tokenDecoder : Decoder String
tokenDecoder =
    Decode.field "token" Decode.string
