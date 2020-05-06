module Wizard.Common.Api.Registry exposing
    ( postConfirmation
    , postSignup
    )

import Json.Encode as E
import Wizard.Common.Api exposing (ToMsg, jwtPost)
import Wizard.Common.AppState exposing (AppState)


postSignup : E.Value -> AppState -> ToMsg () msg -> Cmd msg
postSignup =
    jwtPost "/registry/signup"


postConfirmation : String -> String -> AppState -> ToMsg () msg -> Cmd msg
postConfirmation organizationId hash =
    let
        body =
            E.object
                [ ( "organizationId", E.string organizationId )
                , ( "hash", E.string hash )
                ]
    in
    jwtPost "/registry/confirmation" body
