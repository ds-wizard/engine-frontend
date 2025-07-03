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
import Shared.Components.FontAwesome exposing (fa, faLoginExternalService)
import Wizard.Api.Models.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig exposing (OpenIDServiceConfig)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)


defaultBackground : String
defaultBackground =
    "#333"


defaultColor : String
defaultColor =
    "#fff"


defaultIcon : String
defaultIcon =
    "fa-openid"


type alias ViewConfig msg =
    { onClick : msg
    , service : OpenIDServiceConfig
    }


view : ViewConfig msg -> Html msg
view cfg =
    render
        [ onClick cfg.onClick, dataCy ("login_external_" ++ cfg.service.id) ]
        cfg.service.name
        cfg.service.style.icon
        cfg.service.style.color
        cfg.service.style.background


badgeWrapper : AppState -> String -> Html msg
badgeWrapper appState sourceId =
    case List.find (.id >> (==) sourceId) appState.config.authentication.external.services of
        Just service ->
            viewAsBadge service

        Nothing ->
            Badge.light [] [ text sourceId ]


viewAsBadge : OpenIDServiceConfig -> Html msg
viewAsBadge config =
    Badge.badge
        [ color config.style.color
        , background config.style.background
        ]
        [ icon config.style.icon, text config.name ]


render : List (Attribute msg) -> String -> Maybe String -> Maybe String -> Maybe String -> Html msg
render attributes name mbIcon mbColor mbBackground =
    a
        ([ class "btn btn-external-login with-icon"
         , color mbColor
         , background mbBackground
         ]
            ++ attributes
        )
        [ icon mbIcon, text name ]


background : Maybe String -> Attribute msg
background =
    style "background" << Maybe.withDefault defaultBackground


color : Maybe String -> Attribute msg
color =
    style "color" << Maybe.withDefault defaultColor


icon : Maybe String -> Html msg
icon =
    Maybe.withDefault faLoginExternalService << Maybe.map (\i -> fa i)
