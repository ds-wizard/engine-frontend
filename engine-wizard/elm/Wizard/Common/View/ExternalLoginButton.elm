module Wizard.Common.View.ExternalLoginButton exposing (badgeWrapper, defaultBackground, defaultColor, defaultIcon, preview, view, viewAsBadge)

import Html exposing (Attribute, Html, a, span, text)
import Html.Attributes exposing (class, href, style)
import List.Extra as List
import Shared.Html exposing (fa, faKey, faSet)
import Wizard.Common.Api.Auth as AuthApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Config.Partials.OpenIDServiceConfig exposing (OpenIDServiceConfig)


defaultBackground : String
defaultBackground =
    "#333"


defaultColor : String
defaultColor =
    "#fff"


defaultIcon : AppState -> String
defaultIcon appState =
    Maybe.withDefault "" <| faKey "login.externalService" appState


view : AppState -> OpenIDServiceConfig -> Html msg
view appState config =
    render
        [ href <| AuthApi.authRedirectUrl config.id appState ]
        appState
        config.name
        config.style.icon
        config.style.color
        config.style.background


preview : AppState -> String -> Maybe String -> Maybe String -> Maybe String -> Html msg
preview =
    render []


badgeWrapper : AppState -> String -> Html msg
badgeWrapper appState sourceId =
    case List.find (.id >> (==) sourceId) appState.config.authentication.external.services of
        Just service ->
            viewAsBadge appState service

        Nothing ->
            span [ class "badge badge-external-service badge-light" ] [ text sourceId ]


viewAsBadge : AppState -> OpenIDServiceConfig -> Html msg
viewAsBadge appState config =
    span
        [ class "badge badge-external-service"
        , color config.style.color
        , background config.style.background
        ]
        [ icon appState config.style.icon, text config.name ]


render : List (Attribute msg) -> AppState -> String -> Maybe String -> Maybe String -> Maybe String -> Html msg
render attributes appState name mbIcon mbColor mbBackground =
    a
        ([ class "btn btn-external-login"
         , color mbColor
         , background mbBackground
         ]
            ++ attributes
        )
        [ icon appState mbIcon, text name ]


background : Maybe String -> Attribute msg
background =
    style "background" << Maybe.withDefault defaultBackground


color : Maybe String -> Attribute msg
color =
    style "color" << Maybe.withDefault defaultColor


icon : AppState -> Maybe String -> Html msg
icon appState =
    Maybe.withDefault (faSet "login.externalService" appState) << Maybe.map (\i -> fa i)
