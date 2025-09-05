module Wizard.Components.ExternalLoginButton exposing
    ( ViewConfig
    , badgeWrapper
    , defaultBackground
    , defaultColor
    , defaultIcon
    , render
    , view
    )

import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (fa, faLoginExternalService)
import Gettext exposing (gettext)
import Html exposing (Attribute, Html, a, text)
import Html.Attributes exposing (class, style)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import Uuid
import Wizard.Api.Models.BootstrapConfig exposing (BootstrapConfig)
import Wizard.Api.Models.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig exposing (OpenIDServiceConfig)


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


badgeWrapper : Gettext.Locale -> BootstrapConfig -> String -> Html msg
badgeWrapper locale config sourceId =
    if sourceId == Uuid.toString Uuid.nil then
        Badge.light [] [ text (gettext "internal" locale) ]

    else
        List.find (.id >> (==) sourceId) config.authentication.external.services
            |> Maybe.unwrap Html.nothing viewAsBadge


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
