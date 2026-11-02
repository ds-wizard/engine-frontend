module Common.Components.UserAvatar exposing
    ( anonymous
    , emailHash
    , file
    , group
    , item
    , person
    , tenant
    , user
    )

{-| The round picture beside a user, a group, a tenant or any other named item,
as a `Ui.ItemIcon` — size it with `ItemIcon.withSize`, draw it with
`ItemIcon.toHtml`.

The picture is decorative in every one of these. Wherever an avatar appears the
name is beside it in text, so an image with alternative text would have the
screen reader say the name twice; `Ui.ItemIcon` renders the image with an empty
`alt` and the initials `aria-hidden`. An avatar with no name next to it — a
stack of faces — is `Ui.AvatarGroup`, which names each one.

These take plain records rather than a user or a member type, so any
application's model can be passed with the fields it has.

-}

import Common.Utils.FileIcon as FileIcon
import Gravatar
import Maybe.Extra as Maybe
import Ui.AvatarGroup as AvatarGroup
import Ui.Icon as Icon
import Ui.ItemIcon as ItemIcon exposing (ItemIcon)


{-| A person: their uploaded picture, else their Gravatar over their initials.

The uploaded picture is used whatever it is. The Gravatar is asked for with a
transparent image where there is none (`d=blank`) and laid over the initials —
first name and last name, one letter each, over a color derived from the name —
so somebody without one shows their initials, as somebody without a hash does,
rather than a generic silhouette that tells nobody apart.

-}
user : { firstName : String, lastName : String, imageUrl : Maybe String, gravatarHash : Maybe String } -> ItemIcon
user config =
    let
        picture =
            userPicture config
    in
    ItemIcon.custom
        { name = String.trim (config.firstName ++ " " ++ config.lastName)
        , initials = String.left 1 config.firstName ++ String.left 1 config.lastName
        }
        |> ItemIcon.withImage picture.image
        |> ItemIcon.withImageOver picture.over


{-| The same person as one of a row of faces (`Ui.AvatarGroup`), pictured by
the same rule.
-}
person : { firstName : String, lastName : String, imageUrl : Maybe String, gravatarHash : Maybe String } -> AvatarGroup.Person
person config =
    let
        picture =
            userPicture config

        withMaybe f value face =
            Maybe.unwrap face (\v -> f v face) value
    in
    AvatarGroup.person (String.trim (config.firstName ++ " " ++ config.lastName))
        |> withMaybe AvatarGroup.withImage picture.image
        |> withMaybe AvatarGroup.withImageOver picture.over


{-| The Gravatar hash of an address, for a model that has the e-mail but not
the hash the server computes.
-}
emailHash : String -> Maybe String
emailHash email =
    if String.isEmpty (String.trim email) then
        Nothing

    else
        Just (Gravatar.hashEmail email)


userPicture : { a | imageUrl : Maybe String, gravatarHash : Maybe String } -> { image : Maybe String, over : Maybe String }
userPicture config =
    case config.imageUrl of
        Just _ ->
            { image = config.imageUrl, over = Nothing }

        Nothing ->
            { image = Nothing
            , over =
                config.gravatarHash
                    |> Maybe.filter (not << String.isEmpty)
                    |> Maybe.map (Gravatar.urlFromHash (Gravatar.withDefault Gravatar.Blank Gravatar.defaultOptions))
            }


{-| Somebody who is not signed in: the outline of a person on a quiet gray
ground, the same wherever they appear. There is no name to take an initial or a
color from, and the initial of "Anonymous user" would say the opposite — that
this is somebody in particular.
-}
anonymous : ItemIcon
anonymous =
    ItemIcon.view ""
        |> ItemIcon.withGlyph (Icon.solid "fa-user")
        |> ItemIcon.withPlainGround


{-| A file: the kind of file rather than the first letter of whatever it was
called, and the kind is what the color is derived from too, so every file of a
kind looks alike.
-}
file : { a | fileName : String, contentType : String } -> ItemIcon
file config =
    let
        iconClass =
            FileIcon.getFileIcon config.fileName config.contentType
    in
    ItemIcon.view iconClass
        |> ItemIcon.withGlyph (Icon.fromClass iconClass)


{-| A user group. Groups have no picture, so it is always the initial.
-}
group : String -> ItemIcon
group name =
    ItemIcon.view name


{-| A tenant: its logo when it has one, its initial otherwise.
-}
tenant : { name : String, logoUrl : Maybe String } -> ItemIcon
tenant config =
    ItemIcon.view config.name
        |> ItemIcon.withImage config.logoUrl


{-| Anything else with a name and perhaps a picture — a knowledge model, a
document template, a project.
-}
item : String -> Maybe String -> ItemIcon
item name image =
    ItemIcon.view name
        |> ItemIcon.withImage image
