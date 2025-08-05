module Wizard.Common.Components.OnlineUser exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div, img)
import Html.Attributes exposing (class, src)
import List.Extra as List
import Shared.Components.FontAwesome exposing (fa)
import Wizard.Api.Models.OnlineUserInfo as OnlineUserInfo exposing (LoggedData, OnlineUserInfo)
import Wizard.Api.Models.User as User
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (tooltip, tooltipLeft)


view : AppState -> Bool -> OnlineUserInfo -> Html msg
view appState isTooltipLeft userInfo =
    let
        ( username, colorClass, content ) =
            case userInfo of
                OnlineUserInfo.Logged data ->
                    viewLogged data

                OnlineUserInfo.Anonymous { avatarNumber, colorNumber } ->
                    viewAnonymous appState avatarNumber colorNumber

        tooltipAttributes =
            if isTooltipLeft then
                tooltipLeft

            else
                tooltip
    in
    div (class "OnlineUser" :: class colorClass :: tooltipAttributes username) [ content ]


viewLogged : LoggedData -> ( String, String, Html msg )
viewLogged userData =
    ( User.fullName userData
    , "color-" ++ String.fromInt userData.colorNumber
    , div [ class "Logged" ] [ img [ src (User.imageUrlOrGravatar userData) ] [] ]
    )


viewAnonymous : AppState -> Int -> Int -> ( String, String, Html msg )
viewAnonymous appState avatarNumber colorNumber =
    let
        avatars =
            getAvatars appState

        ( avatarIcon, avatarName ) =
            List.getAt (modBy (List.length avatars) avatarNumber) avatars
                |> Maybe.withDefault ( "fa-cat", gettext "Anonymous Cat" appState.locale )
    in
    ( avatarName
    , "color-" ++ String.fromInt colorNumber
    , div [ class "Anonymous" ] [ fa ("fa-lg " ++ avatarIcon) ]
    )


getAvatars : AppState -> List ( String, String )
getAvatars appState =
    [ ( "fa-cat", gettext "Anonymous Cat" appState.locale )
    , ( "fa-crow", gettext "Anonymous Crow" appState.locale )
    , ( "fa-dog", gettext "Anonymous Dog" appState.locale )
    , ( "fa-dove", gettext "Anonymous Dove" appState.locale )
    , ( "fa-dragon", gettext "Anonymous Dragon" appState.locale )
    , ( "fa-fish", gettext "Anonymous Fish" appState.locale )
    , ( "fa-frog", gettext "Anonymous Frog" appState.locale )
    , ( "fa-hippo", gettext "Anonymous Hippo" appState.locale )
    , ( "fa-horse", gettext "Anonymous Horse" appState.locale )
    , ( "fa-kiwi-bird", gettext "Anonymous Kiwi" appState.locale )
    , ( "fa-otter", gettext "Anonymous Otter" appState.locale )
    , ( "fa-spider", gettext "Anonymous Spider" appState.locale )
    , ( "fa-piggy-bank", gettext "Anonymous Pig" appState.locale )
    , ( "fa-bug", gettext "Anonymous Bug" appState.locale )
    , ( "fa-hat-wizard", gettext "Anonymous Wizard" appState.locale )
    , ( "fa-ghost", gettext "Anonymous Ghost" appState.locale )
    , ( "fa-robot", gettext "Anonymous Robot" appState.locale )
    , ( "fa-snowman", gettext "Anonymous Snowman" appState.locale )
    , ( "fa-tree", gettext "Anonymous Tree" appState.locale )
    , ( "fa-hat-cowboy", gettext "Anonymous Cowboy" appState.locale )
    ]
