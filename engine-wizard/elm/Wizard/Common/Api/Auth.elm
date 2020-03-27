module Wizard.Common.Api.Auth exposing
    ( authRedirectUrl
    , getToken
    )

import Json.Decode as D exposing (Decoder)
import Wizard.Common.Api exposing (ToMsg, httpGet)
import Wizard.Common.AppState exposing (AppState)


getToken : String -> Maybe String -> Maybe String -> AppState -> ToMsg String msg -> Cmd msg
getToken id error code =
    let
        decoder =
            D.field "token" D.string
    in
    httpGet
        ("/auth/" ++ id ++ "/callback?error=" ++ Maybe.withDefault "" error ++ "&code=" ++ Maybe.withDefault "" code)
        decoder


authRedirectUrl : String -> AppState -> String
authRedirectUrl id appState =
    appState.apiUrl ++ "/auth/" ++ id
