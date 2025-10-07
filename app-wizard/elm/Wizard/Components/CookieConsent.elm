module Wizard.Components.CookieConsent exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, a, br, button, div, text)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Wizard.Data.AppState exposing (AppState)
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
                    [ text (gettext "Privacy & Cookies: This site uses cookies. By continuing to use this website, you agree to their use. " appState.locale)
                    , br [] []
                    , text (gettext "To find out more, including how to control cookies, see here: " appState.locale)
                    , a [ href cookiePolicyUrl, target "_blank" ] [ text (gettext "Cookie Policy" appState.locale) ]
                    ]
                , button [ class "btn btn-primary", onClick AcceptCookies ] [ text (gettext "Close and Accept" appState.locale) ]
                ]
            ]

    else
        Html.nothing
