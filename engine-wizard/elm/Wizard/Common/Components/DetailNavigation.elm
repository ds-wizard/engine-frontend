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
import Shared.Data.OnlineUserInfo exposing (OnlineUserInfo)
import Shared.Html exposing (emptyNode)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.OnlineUser as OnlineUser
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
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


onlineUsers : AppState -> List OnlineUserInfo -> Html msg
onlineUsers appState users =
    if List.isEmpty users then
        emptyNode

    else
        let
            extraUsers =
                if List.length users > 10 then
                    div [ class "extra-users-count" ]
                        [ text ("+" ++ String.fromInt (List.length users - 10)) ]

                else
                    emptyNode
        in
        div
            [ class "DetailNavigation__Row__Section__Online-Users"
            , classList [ ( "DetailNavigation__Row__Section__Online-Users--Stacked", List.length users > 5 ) ]
            ]
            (List.map (OnlineUser.view appState) (List.take 10 users)
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


navLink : AppState -> NavLinkConfig msg -> Html msg
navLink appState cfg =
    if cfg.isVisible then
        li [ class "nav-item" ]
            [ linkTo appState
                cfg.route
                [ class "nav-link", classList [ ( "active", cfg.isActive ) ], dataCy cfg.dataCy ]
                [ cfg.icon
                , span [ attribute "data-content" cfg.label ] [ text cfg.label ]
                ]
            ]

    else
        emptyNode


navigation : AppState -> List (NavLinkConfig msg) -> Html msg
navigation appState cfgs =
    div [ class "DetailNavigation__Row" ]
        [ ul [ class "nav nav-underline-tabs" ] (List.map (navLink appState) cfgs) ]
