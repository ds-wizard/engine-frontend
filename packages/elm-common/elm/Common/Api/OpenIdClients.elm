module Common.Api.OpenIdClients exposing
    ( deleteOpenIdClient
    , getOpenIdClient
    , getOpenIdClients
    , getToken
    , postOpenIdClient
    , putOpenIdClient
    , request
    )

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.OpenIdClient as OpenIdClient exposing (OpenIdClient)
import Common.Api.Models.OpenIdClientDetail as OpenIdClientDetail exposing (OpenIdClientDetail)
import Common.Api.Models.OpenIdRequest as OpenIdRequestResponse exposing (OpenIdRequestResponse)
import Common.Api.Models.TokenResponse as TokenResponse exposing (TokenResponse)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Utils.UrlUtils as UrlUtils
import Json.Decode as D exposing (Decoder)
import Uuid exposing (Uuid)


getOpenIdClients : Context ctx -> ToMsg (List OpenIdClient) msg -> Cmd msg
getOpenIdClients appState =
    Request.get (Context.toServerInfo appState) "/open-id-clients" (D.list OpenIdClient.decoder)


{-| With whatever else `extra` reads from the same detail.
-}
getOpenIdClient : Context ctx -> Uuid -> Decoder extra -> ToMsg ( OpenIdClientDetail, extra ) msg -> Cmd msg
getOpenIdClient appState uuid extra =
    Request.get (Context.toServerInfo appState) ("/open-id-clients/" ++ Uuid.toString uuid) (D.map2 Tuple.pair OpenIdClientDetail.decoder extra)


postOpenIdClient : Context ctx -> OpenIdClientDetail -> ToMsg OpenIdClient msg -> Cmd msg
postOpenIdClient appState config =
    let
        body =
            OpenIdClientDetail.encodeNew config
    in
    Request.post (Context.toServerInfo appState) "/open-id-clients" OpenIdClient.decoder body


putOpenIdClient : Context ctx -> OpenIdClientDetail -> ToMsg () msg -> Cmd msg
putOpenIdClient appState config =
    let
        body =
            OpenIdClientDetail.encode config
    in
    Request.putWhatever (Context.toServerInfo appState) ("/open-id-clients/" ++ Uuid.toString config.uuid) body


deleteOpenIdClient : Context ctx -> Uuid -> ToMsg () msg -> Cmd msg
deleteOpenIdClient appState uuid =
    Request.delete (Context.toServerInfo appState) ("/open-id-clients/" ++ Uuid.toString uuid)


request : Context ctx -> OpenIdClient -> ToMsg OpenIdRequestResponse msg -> Cmd msg
request appState config =
    Request.get (Context.toServerInfo appState) ("/open-id-clients/" ++ Uuid.toString config.uuid ++ "/request") OpenIdRequestResponse.decoder


getToken : Context ctx -> String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> ToMsg TokenResponse msg -> Cmd msg
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
    Request.get (Context.toServerInfo appState) url TokenResponse.decoder
