module Common.Components.OnlineUsers exposing (avatars)

{-| The people in a project or a knowledge model editor at the same time as you,
as faces for `Ui.AvatarGroup`.

**You are not one of them.** The list the server sends includes whoever asked
for it, and a face of your own among the others says nothing; the same person
open in two tabs is one face, not two.

`avatars` is `Nothing` when nobody else is there, so the page can leave the
group — and the rule that separates it from the actions — out altogether rather
than drawing an empty one.

A logged-in person is their picture and their name. Somebody anonymous is told
apart by the animal the server gave them — its icon is their face and its name
their label, as in the wizard — on the hue their color number names.

-}

import Common.Api.Models.OnlineUserInfo as OnlineUserInfo exposing (OnlineUserInfo)
import Common.Components.UiWords as UiWords
import Common.Components.UserAvatar as UserAvatar
import Gettext exposing (gettext)
import Html exposing (Html)
import List.Extra as List
import Ui.AvatarGroup as AvatarGroup
import Ui.Icon as Icon
import Uuid


avatars :
    { a | locale : Gettext.Locale, config : { b | user : Maybe { c | uuid : Uuid.Uuid } } }
    -> List OnlineUserInfo
    -> Maybe (Html msg)
avatars appState users =
    let
        currentUserUuid =
            Maybe.map .uuid appState.config.user
                |> Maybe.withDefault Uuid.nil

        unique accumulator remaining =
            case remaining of
                [] ->
                    List.reverse accumulator

                user :: rest ->
                    if List.any (OnlineUserInfo.matchUuid (Maybe.withDefault Uuid.nil (OnlineUserInfo.getUuid user))) accumulator then
                        unique accumulator rest

                    else
                        unique (user :: accumulator) rest

        others =
            users
                |> List.filterNot (OnlineUserInfo.matchUuid currentUserUuid)
                |> unique []
    in
    if List.isEmpty others then
        Nothing

    else
        Just
            (AvatarGroup.toHtmlWithWords (UiWords.avatarGroup appState)
                { label = gettext "Online users" appState.locale
                , visible = 10
                , people = List.map (person appState) others
                }
            )


person : { a | locale : Gettext.Locale } -> OnlineUserInfo -> AvatarGroup.Person
person appState userInfo =
    case userInfo of
        OnlineUserInfo.Logged user ->
            UserAvatar.person
                { firstName = user.firstName
                , lastName = user.lastName
                , imageUrl = user.imageUrl
                , gravatarHash = Just user.gravatarHash
                }

        OnlineUserInfo.Anonymous { avatarNumber, colorNumber } ->
            -- An anonymous visitor is told apart by the animal they were given:
            -- its icon is their face and its name their label, as in the wizard,
            -- on the color the server gave them (`anonymousHue`).
            let
                ( icon, name ) =
                    anonymousAvatar appState avatarNumber
            in
            AvatarGroup.person name
                |> AvatarGroup.withGlyph (Icon.solid icon)
                |> AvatarGroup.withHue (anonymousHue colorNumber)


{-| Twelve user colors as twelve hues 30° apart, in an order that
puts neighboring color numbers far apart on the wheel (150° a step), so the
first few people online are as different as they can be.
-}
anonymousHue : Int -> Float
anonymousHue colorNumber =
    toFloat (modBy 12 (colorNumber * 5) * 30)


anonymousAvatar : { a | locale : Gettext.Locale } -> Int -> ( String, String )
anonymousAvatar appState avatarNumber =
    let
        animals =
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
    in
    List.getAt (modBy (List.length animals) avatarNumber) animals
        |> Maybe.withDefault ( "fa-cat", gettext "Anonymous Cat" appState.locale )
