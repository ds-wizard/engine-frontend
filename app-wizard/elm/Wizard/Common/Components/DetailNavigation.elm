module Wizard.Common.Components.DetailNavigation exposing
    ( NavLinkConfig
    , container
    , navigation
    , onlineUsers
    , row
    , section
    , sectionActions
    )

import Html exposing (Html, div, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList)
import Html.Extra as Html
import Wizard.Api.Models.OnlineUserInfo exposing (OnlineUserInfo)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.OnlineUser as OnlineUser
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, dataTour)
import Wizard.Routes


container : List (Html msg) -> Html msg
container =
    div [ class "DetailNavigation" ]


row : List (Html msg) -> Html msg
row =
    div [ class "DetailNavigation__Row" ]


section : List (Html msg) -> Html msg
section =
    div [ class "DetailNavigation__Row__Section" ]


sectionActions : List (Html msg) -> Html msg
sectionActions =
    div [ class "DetailNavigation__Row__Section__Actions" ]


onlineUsers : AppState -> Bool -> List OnlineUserInfo -> Html msg
onlineUsers appState isTooltipLeft users =
    if List.isEmpty users then
        Html.nothing

    else
        let
            extraUsers =
                if List.length users > 10 then
                    div [ class "extra-users-count" ]
                        [ text ("+" ++ String.fromInt (List.length users - 10)) ]

                else
                    Html.nothing
        in
        div
            [ class "DetailNavigation__Row__Section__Online-Users"
            , classList [ ( "DetailNavigation__Row__Section__Online-Users--Stacked", List.length users > 5 ) ]
            ]
            (List.map (OnlineUser.view appState isTooltipLeft) (List.take 10 users)
                ++ [ extraUsers ]
            )


type alias NavLinkConfig msg =
    { route : Wizard.Routes.Route
    , label : String
    , icon : Html msg
    , isActive : Bool
    , isVisible : Bool
    , dataCy : String
    }


navLink : NavLinkConfig msg -> Html msg
navLink cfg =
    if cfg.isVisible then
        li [ class "nav-item" ]
            [ linkTo cfg.route
                [ class "nav-link", classList [ ( "active", cfg.isActive ) ], dataCy cfg.dataCy ]
                [ cfg.icon
                , span [ attribute "data-content" cfg.label ] [ text cfg.label ]
                ]
            ]

    else
        Html.nothing


navigation : List (NavLinkConfig msg) -> Html msg
navigation cfgs =
    row
        [ ul [ class "nav nav-underline-tabs", dataTour "navigation" ] (List.map navLink cfgs) ]
