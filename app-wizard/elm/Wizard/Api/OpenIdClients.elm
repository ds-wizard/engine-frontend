module Wizard.Api.OpenIdClients exposing
    ( deleteOpenIdClient
    , getOpenIdClient
    , getOpenIdClients
    , getToken
    , postOpenIdClient
    , putOpenIdClient
    , request
    )

import Common.Api.Models.OpenIdClient as OpenIdClient exposing (OpenIdClient)
import Common.Api.Models.OpenIdRequest as OpenIdRequestResponse exposing (OpenIdRequestResponse)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Utils.UrlUtils as UrlUtils
import Json.Decode as D
import Uuid exposing (Uuid)
import Wizard.Api.Models.OpenIdClientDetail as OpenIdClientDetail exposing (OpenIdClientDetail)
import Wizard.Api.Models.TokenResponse as TokenResponse exposing (TokenResponse)
import Wizard.Data.AppState as AppState exposing (AppState)


getOpenIdClients : AppState -> ToMsg (List OpenIdClient) msg -> Cmd msg
getOpenIdClients appState =
    Request.get (AppState.toServerInfo appState) "/open-id-clients" (D.list OpenIdClient.decoder)


getOpenIdClient : AppState -> Uuid -> ToMsg OpenIdClientDetail msg -> Cmd msg
getOpenIdClient appState uuid =
    Request.get (AppState.toServerInfo appState) ("/open-id-clients/" ++ Uuid.toString uuid) OpenIdClientDetail.decoder


postOpenIdClient : AppState -> OpenIdClientDetail -> ToMsg OpenIdClient msg -> Cmd msg
postOpenIdClient appState config =
    let
        body =
            OpenIdClientDetail.encodeNew config
    in
    Request.post (AppState.toServerInfo appState) "/open-id-clients" OpenIdClient.decoder body


putOpenIdClient : AppState -> OpenIdClientDetail -> ToMsg () msg -> Cmd msg
putOpenIdClient appState config =
    let
        body =
            OpenIdClientDetail.encode config
    in
    Request.putWhatever (AppState.toServerInfo appState) ("/open-id-clients/" ++ Uuid.toString config.uuid) body


deleteOpenIdClient : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteOpenIdClient appState uuid =
    Request.delete (AppState.toServerInfo appState) ("/open-id-clients/" ++ Uuid.toString uuid)


request : AppState -> OpenIdClient -> ToMsg OpenIdRequestResponse msg -> Cmd msg
request appState config =
    Request.get (AppState.toServerInfo appState) ("/open-id-clients/" ++ Uuid.toString config.uuid ++ "/request") OpenIdRequestResponse.decoder


getToken : AppState -> String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> ToMsg TokenResponse msg -> Cmd msg
getToken appState id mbError mbCode mbSessionState mbState =
    let
        queryParams =
            UrlUtils.queryParamsToString
                [ ( "error", mbError )
                , ( "code", mbCode )
                , ( "session_state", mbSessionState )
                , ( "state", mbState )
                ]

        url =
            "/open-id-clients/" ++ id ++ "/response" ++ queryParams
    in
    Request.get (AppState.toServerInfo appState) url TokenResponse.decoder
