module Wizard.Pages.Public.Subscriptions exposing (subscriptions)

import Wizard.Pages.Public.Auth.Subscriptions
import Wizard.Pages.Public.Msgs exposing (Msg(..))
import Wizard.Pages.Public.Routes exposing (Route(..))


subscriptions : Route -> Sub Msg
subscriptions route =
    case route of
        AuthCallback _ _ _ _ ->
            Sub.map AuthMsg <|
                Wizard.Pages.Public.Auth.Subscriptions.subscriptions

        _ ->
            Sub.none
