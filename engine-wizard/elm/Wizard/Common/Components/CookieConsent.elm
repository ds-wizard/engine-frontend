module Wizard.Common.Components.CookieConsent exposing (view)

import Html exposing (Html, a, br, button, div, text)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Shared.Html exposing (emptyNode)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs exposing (Msg(..))


cookiePolicyUrl : String
cookiePolicyUrl =
    "https://ds-wizard.org/cookie-policy.html"


view : AppState -> Html Msg
view appState =
    let
        visible =
            appState.gaEnabled && not appState.cookieConsent
    in
    if visible then
        div [ class "CookieConsent" ]
            [ div [ class "container" ]
                [ div []
                    [ text "Privacy & Cookies: This site uses cookies. By continuing to use this website, you agree to their use. "
                    , br [] []
                    , text "To find out more, including how to control cookies, see here: "
                    , a [ href cookiePolicyUrl, target "_blank" ] [ text "Cookie Policy" ]
                    ]
                , button [ class "btn btn-primary", onClick AcceptCookies ] [ text "Close and Accept" ]
                ]
            ]

    else
        emptyNode
