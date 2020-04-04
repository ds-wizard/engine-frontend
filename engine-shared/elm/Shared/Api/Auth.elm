module Shared.Api.Auth exposing (authRedirectUrl, getToken)

import Shared.Api exposing (AppStateLike, ToMsg, httpGet)
import Shared.Data.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig as OpenIDServiceConfig exposing (OpenIDServiceConfig)
import Shared.Data.Token as Token exposing (Token)
import String.Extra as String


getToken : String -> Maybe String -> Maybe String -> AppStateLike a -> ToMsg Token msg -> Cmd msg
getToken id mbError mbCode =
    httpGet ("/auth/" ++ id ++ "/callback?error=" ++ String.fromMaybe mbError ++ "&code=" ++ String.fromMaybe mbCode) Token.decoder


authRedirectUrl : OpenIDServiceConfig -> AppStateLike a -> String
authRedirectUrl config appState =
    appState.apiConfig.apiUrl ++ "/auth/" ++ OpenIDServiceConfig.id config
