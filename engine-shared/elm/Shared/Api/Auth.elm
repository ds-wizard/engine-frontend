module Shared.Api.Auth exposing (authRedirectUrl, getToken)

import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, httpGet)
import Shared.Data.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig exposing (OpenIDServiceConfig)
import Shared.Data.TokenResponse as TokenResponse exposing (TokenResponse)
import String.Extra as String


getToken : String -> Maybe String -> Maybe String -> Maybe String -> AbstractAppState a -> ToMsg TokenResponse msg -> Cmd msg
getToken id mbError mbCode mbSessionState =
    httpGet ("/auth/" ++ id ++ "/callback?error=" ++ String.fromMaybe mbError ++ "&code=" ++ String.fromMaybe mbCode ++ "&session_state=" ++ String.fromMaybe mbSessionState ++ "&nonce=FtEIbRdfFc7z2bNjCTaZKDcWNeUKUelvs13K21VL") TokenResponse.decoder


authRedirectUrl : OpenIDServiceConfig -> AbstractAppState a -> String
authRedirectUrl config appState =
    appState.apiUrl ++ "/auth/" ++ config.id
