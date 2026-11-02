module Common.Api.Tokens exposing
    ( deleteCurrentToken
    , deleteToken
    , deleteTokens
    , fetchToken
    , fetchTokenWithCaptcha
    , getTokens
    )

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.ApiKey as ApiKey exposing (ApiKey)
import Common.Api.Models.TokenResponse as TokenResponse exposing (TokenResponse)
import Common.Api.Request as Request exposing (ToMsg)
import Http
import Json.Decode as D
import Json.Encode as E
import Uuid exposing (Uuid)


fetchToken : Context ctx -> E.Value -> ToMsg TokenResponse msg -> Cmd msg
fetchToken appState body =
    Request.post (Context.toServerInfo appState) "/tokens" TokenResponse.decoder body


{-| `fetchToken` with the token of a solved captcha (`Common.Ports.Captcha`),
when there is one, in the header the server's firewall reads it from.
-}
fetchTokenWithCaptcha : Context ctx -> Maybe String -> E.Value -> ToMsg TokenResponse msg -> Cmd msg
fetchTokenWithCaptcha appState mbCaptchaToken body toMsg =
    case mbCaptchaToken of
        Just captchaToken ->
            let
                serverInfo =
                    Context.toServerInfo appState
            in
            Http.request
                { method = "POST"
                , headers = Http.header "x-aws-waf-token" captchaToken :: Request.authorizationHeaders serverInfo
                , url = serverInfo.apiUrl ++ "/tokens"
                , body = Http.jsonBody body
                , expect = Request.expectJson toMsg TokenResponse.decoder
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            fetchToken appState body toMsg


getTokens : Context ctx -> ToMsg (List ApiKey) msg -> Cmd msg
getTokens appState =
    Request.get (Context.toServerInfo appState) "/tokens" (D.list ApiKey.decoder)


deleteToken : Context ctx -> Uuid -> ToMsg () msg -> Cmd msg
deleteToken appState uuid =
    Request.delete (Context.toServerInfo appState) ("/tokens/" ++ Uuid.toString uuid)


deleteCurrentToken : Context ctx -> ToMsg () msg -> Cmd msg
deleteCurrentToken appState =
    Request.delete (Context.toServerInfo appState) "/tokens/current"


deleteTokens : Context ctx -> ToMsg () msg -> Cmd msg
deleteTokens appState =
    Request.delete (Context.toServerInfo appState) "/tokens"
