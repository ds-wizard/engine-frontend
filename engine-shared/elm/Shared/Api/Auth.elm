module Shared.Api.Auth exposing (authRedirectUrl, getToken)

import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, httpGet)
import Shared.Data.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig exposing (OpenIDServiceConfig)
import Shared.Data.Token as Token exposing (Token)
import String.Extra as String


getToken : String -> Maybe String -> Maybe String -> AbstractAppState a -> ToMsg Token msg -> Cmd msg
getToken id mbError mbCode =
    httpGet ("/auth/" ++ id ++ "/callback?error=" ++ String.fromMaybe mbError ++ "&code=" ++ String.fromMaybe mbCode ++ "&nonce=FtEIbRdfFc7z2bNjCTaZKDcWNeUKUelvs13K21VL") Token.decoder


authRedirectUrl : OpenIDServiceConfig -> AbstractAppState a -> String
authRedirectUrl config appState =
    appState.apiUrl ++ "/auth/" ++ config.id
