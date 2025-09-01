module Wizard.Api.Auth exposing
    ( authRedirectUrl
    , getToken
    , postConsents
    )

import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Api.Request as Request exposing (ToMsg)
import String.Extra as String
import Wizard.Api.Models.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig exposing (OpenIDServiceConfig)
import Wizard.Api.Models.TokenResponse as TokenResponse exposing (TokenResponse)
import Wizard.Data.AppState as AppState exposing (AppState)


getToken : AppState -> String -> Maybe String -> Maybe String -> Maybe String -> ToMsg TokenResponse msg -> Cmd msg
getToken appState id mbError mbCode mbSessionState =
    let
        url =
            "/auth/"
                ++ id
                ++ "/callback?error="
                ++ String.fromMaybe mbError
                ++ "&code="
                ++ String.fromMaybe mbCode
                ++ "&session_state="
                ++ String.fromMaybe mbSessionState
                ++ "&nonce=FtEIbRdfFc7z2bNjCTaZKDcWNeUKUelvs13K21VL"
    in
    Request.get (AppState.toServerInfo appState) url TokenResponse.decoder


authRedirectUrl : AppState -> OpenIDServiceConfig -> String
authRedirectUrl appState config =
    appState.apiUrl ++ "/auth/" ++ config.id


postConsents : AppState -> String -> String -> Maybe String -> ToMsg TokenResponse msg -> Cmd msg
postConsents appState id hash mbSessionState =
    let
        body =
            E.object
                [ ( "hash", E.string hash )
                , ( "sessionState", E.maybe E.string mbSessionState )
                ]
    in
    Request.post (AppState.toServerInfo appState) ("/auth/" ++ id ++ "/consents") TokenResponse.decoder body
