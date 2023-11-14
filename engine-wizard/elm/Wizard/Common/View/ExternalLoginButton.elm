module Wizard.Common.View.ExternalLoginButton exposing
    ( ViewConfig
    , badgeWrapper
    , defaultBackground
    , defaultColor
    , defaultIcon
    , render
    , view
    )

import Html exposing (Attribute, Html, a, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import List.Extra as List
import Shared.Components.Badge as Badge
import Shared.Data.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig exposing (OpenIDServiceConfig)
import Shared.Html exposing (fa, faKey, faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)


defaultBackground : String
defaultBackground =
    "#333"


defaultColor : String
defaultColor =
    "#fff"


defaultIcon : AppState -> String
defaultIcon appState =
    Maybe.withDefault "" <| faKey "login.externalService" appState


type alias ViewConfig msg =
    { onClick : msg
    , service : OpenIDServiceConfig
    }


view : AppState -> ViewConfig msg -> Html msg
view appState cfg =
    render
        [ onClick cfg.onClick, dataCy ("login_external_" ++ cfg.service.id) ]
        appState
        cfg.service.name
        cfg.service.style.icon
        cfg.service.style.color
        cfg.service.style.background


badgeWrapper : AppState -> String -> Html msg
badgeWrapper appState sourceId =
    case List.find (.id >> (==) sourceId) appState.config.authentication.external.services of
        Just service ->
            viewAsBadge appState service

        Nothing ->
            Badge.light [] [ text sourceId ]


viewAsBadge : AppState -> OpenIDServiceConfig -> Html msg
viewAsBadge appState config =
    Badge.badge
        [ color config.style.color
        , background config.style.background
        ]
        [ icon appState config.style.icon, text config.name ]


render : List (Attribute msg) -> AppState -> String -> Maybe String -> Maybe String -> Maybe String -> Html msg
render attributes appState name mbIcon mbColor mbBackground =
    a
        ([ class "btn btn-external-login with-icon"
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
