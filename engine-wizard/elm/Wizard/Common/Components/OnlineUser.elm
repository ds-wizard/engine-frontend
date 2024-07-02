module Wizard.Common.Components.OnlineUser exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div, img)
import Html.Attributes exposing (class, src)
import List.Extra as List
import Shared.Data.OnlineUserInfo as OnlineUserInfo exposing (LoggedData, OnlineUserInfo)
import Shared.Data.User as User
import Shared.Html exposing (fa, faKeyClass)
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
                |> Maybe.withDefault ( faKeyClass "avatar.0" appState, gettext "Anonymous Cat" appState.locale )
    in
    ( avatarName
    , "color-" ++ String.fromInt colorNumber
    , div [ class "Anonymous" ] [ fa ("fa-lg " ++ avatarIcon) ]
    )


getAvatars : AppState -> List ( String, String )
getAvatars appState =
    [ ( faKeyClass "avatar.0" appState, gettext "Anonymous Cat" appState.locale )
    , ( faKeyClass "avatar.1" appState, gettext "Anonymous Crow" appState.locale )
    , ( faKeyClass "avatar.2" appState, gettext "Anonymous Dog" appState.locale )
    , ( faKeyClass "avatar.3" appState, gettext "Anonymous Dove" appState.locale )
    , ( faKeyClass "avatar.4" appState, gettext "Anonymous Dragon" appState.locale )
    , ( faKeyClass "avatar.5" appState, gettext "Anonymous Fish" appState.locale )
    , ( faKeyClass "avatar.6" appState, gettext "Anonymous Frog" appState.locale )
    , ( faKeyClass "avatar.7" appState, gettext "Anonymous Hippo" appState.locale )
    , ( faKeyClass "avatar.8" appState, gettext "Anonymous Horse" appState.locale )
    , ( faKeyClass "avatar.9" appState, gettext "Anonymous Kiwi" appState.locale )
    , ( faKeyClass "avatar.10" appState, gettext "Anonymous Otter" appState.locale )
    , ( faKeyClass "avatar.11" appState, gettext "Anonymous Spider" appState.locale )
    , ( faKeyClass "avatar.12" appState, gettext "Anonymous Pig" appState.locale )
    , ( faKeyClass "avatar.13" appState, gettext "Anonymous Bug" appState.locale )
    , ( faKeyClass "avatar.14" appState, gettext "Anonymous Wizard" appState.locale )
    , ( faKeyClass "avatar.15" appState, gettext "Anonymous Ghost" appState.locale )
    , ( faKeyClass "avatar.16" appState, gettext "Anonymous Robot" appState.locale )
    , ( faKeyClass "avatar.17" appState, gettext "Anonymous Snowman" appState.locale )
    , ( faKeyClass "avatar.18" appState, gettext "Anonymous Tree" appState.locale )
    , ( faKeyClass "avatar.19" appState, gettext "Anonymous Cowboy" appState.locale )
    ]
