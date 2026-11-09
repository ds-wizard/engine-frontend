module Common.Api.PasswordStrength exposing (postPasswordStrength)

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.PasswordStrength as PasswordStrength exposing (PasswordStrength)
import Common.Api.Request as Request exposing (ToMsg)
import Json.Encode as E


postPasswordStrength : Context ctx -> String -> ToMsg PasswordStrength msg -> Cmd msg
postPasswordStrength appState password =
    let
        body =
            E.object [ ( "password", E.string password ) ]
    in
    Request.post (Context.toServerInfo appState) "/password-strength" PasswordStrength.decoder body
