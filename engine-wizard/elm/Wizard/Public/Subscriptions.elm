module Wizard.Public.Subscriptions exposing (subscriptions)

import Wizard.Public.Auth.Subscriptions
import Wizard.Public.Msgs exposing (Msg(..))
import Wizard.Public.Routes exposing (Route(..))


subscriptions : Route -> Sub Msg
subscriptions route =
    case route of
        AuthCallback _ _ _ _ ->
            Sub.map AuthMsg <|
                Wizard.Public.Auth.Subscriptions.subscriptions

        _ ->
            Sub.none
