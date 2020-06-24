module Shared.Api.Registry exposing
    ( postConfirmation
    , postSignup
    )

import Json.Encode as E
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtPost)


postSignup : E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
postSignup =
    jwtPost "/registry/signup"


postConfirmation : String -> String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
postConfirmation organizationId hash =
    let
        body =
            E.object
                [ ( "organizationId", E.string organizationId )
                , ( "hash", E.string hash )
                ]
    in
    jwtPost "/registry/confirmation" body
