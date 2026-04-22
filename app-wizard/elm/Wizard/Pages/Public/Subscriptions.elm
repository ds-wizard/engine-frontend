module Wizard.Pages.Public.Subscriptions exposing (subscriptions)

import Wizard.Pages.Public.Msgs exposing (Msg(..))
import Wizard.Pages.Public.OpenIdCallback.Subscriptions
import Wizard.Pages.Public.Routes exposing (Route(..))


subscriptions : Route -> Sub Msg
subscriptions route =
    case route of
        OpenIdCallback _ _ _ _ ->
            Sub.map AuthMsg <|
                Wizard.Pages.Public.OpenIdCallback.Subscriptions.subscriptions

        _ ->
            Sub.none
