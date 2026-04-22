module Wizard.Components.ExternalLoginButton exposing
    ( ViewConfig
    , defaultBackground
    , defaultColor
    , defaultIcon
    , render
    , renderIcon
    , view
    , viewAsBadge
    )

import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (fa, faFw, faLoginExternalService)
import Html exposing (Attribute, Html, a, span, text)
import Html.Attributes exposing (class, style)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Uuid
import Wizard.Api.Models.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig exposing (OpenIDServiceConfig)


defaultBackground : String
defaultBackground =
    "#333"


defaultColor : String
defaultColor =
    "#fff"


defaultIcon : String
defaultIcon =
    "fab fa-openid"


type alias ViewConfig msg =
    { onClick : msg
    , service : OpenIDServiceConfig
    }


view : ViewConfig msg -> Html msg
view cfg =
    render
        [ onClick cfg.onClick, dataCy ("login_external_" ++ Uuid.toString cfg.service.uuid) ]
        cfg.service.name
        cfg.service.style.icon
        cfg.service.style.color
        cfg.service.style.background


viewAsBadge : OpenIDServiceConfig -> Html msg
viewAsBadge config =
    Badge.badge
        [ color config.style.color
        , background config.style.background
        ]
        [ fa ("me-1 " ++ Maybe.withDefault defaultIcon config.style.icon)
        , text config.name
        ]


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


renderIcon : Maybe String -> Maybe String -> Maybe String -> Html msg
renderIcon mbIcon mbColor mbBackground =
    span [ color mbColor, background mbBackground, class "px-2 py-1 rounded me-2" ]
        [ iconFw mbIcon ]


background : Maybe String -> Attribute msg
background =
    style "background" << Maybe.withDefault defaultBackground


color : Maybe String -> Attribute msg
color =
    style "color" << Maybe.withDefault defaultColor


icon : Maybe String -> Html msg
icon =
    Maybe.withDefault faLoginExternalService << Maybe.map (\i -> fa i)


iconFw : Maybe String -> Html msg
iconFw =
    Maybe.withDefault (faFw defaultIcon) << Maybe.map (\i -> faFw i)
