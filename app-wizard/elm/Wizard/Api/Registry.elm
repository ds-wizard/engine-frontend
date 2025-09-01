module Wizard.Api.Registry exposing
    ( postConfirmation
    , postSignup
    )

import Json.Encode as E
import Shared.Api.Request as Request exposing (ToMsg)
import Wizard.Data.AppState as AppState exposing (AppState)


postSignup : AppState -> E.Value -> ToMsg () msg -> Cmd msg
postSignup appState body =
    Request.postWhatever (AppState.toServerInfo appState) "/registry/signup" body


postConfirmation : AppState -> String -> String -> ToMsg () msg -> Cmd msg
postConfirmation appState organizationId hash =
    let
        body =
            E.object
                [ ( "organizationId", E.string organizationId )
                , ( "hash", E.string hash )
                ]
    in
    Request.postWhatever (AppState.toServerInfo appState) "/registry/confirmation" body
