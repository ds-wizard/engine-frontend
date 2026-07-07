module Wizard.Components.DetailNavigation exposing
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
import Html.Attributes.Extensions exposing (dataCy, dataTour)
import Html.Extra as Html
import Maybe.Extra as Maybe
import Uuid
import Wizard.Api.Models.OnlineUserInfo as OnlineUser exposing (OnlineUserInfo)
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.OnlineUser as OnlineUser
import Wizard.Data.AppState exposing (AppState)
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
    let
        currentUserUuid =
            Maybe.unwrap Uuid.nil .uuid appState.config.user

        onlineUserUniqueHelp accumulator remaining =
            case remaining of
                [] ->
                    List.reverse accumulator

                user :: rest ->
                    if List.any (OnlineUser.matchUuid (Maybe.withDefault Uuid.nil (OnlineUser.getUuid user))) accumulator then
                        onlineUserUniqueHelp accumulator rest

                    else
                        onlineUserUniqueHelp (user :: accumulator) rest

        filteredUsers =
            List.filter (not << OnlineUser.matchUuid currentUserUuid) users
                |> onlineUserUniqueHelp []
    in
    if List.isEmpty filteredUsers then
        Html.nothing

    else
        let
            extraUsers =
                if List.length filteredUsers > 10 then
                    div [ class "extra-users-count" ]
                        [ text ("+" ++ String.fromInt (List.length filteredUsers - 10)) ]

                else
                    Html.nothing
        in
        div
            [ class "DetailNavigation__Row__Section__Online-Users"
            , classList [ ( "DetailNavigation__Row__Section__Online-Users--Stacked", List.length filteredUsers > 5 ) ]
            ]
            (List.map (OnlineUser.view appState isTooltipLeft) (List.take 10 filteredUsers)
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
