module Ui.ItemIcon exposing
    ( ItemIcon
    , Size(..)
    , custom
    , toHtml
    , view
    , withBrandOf
    , withColor
    , withGlyph
    , withHue
    , withImage
    , withImageOver
    , withPlainGround
    , withSize
    , withSquare
    )

{-| The round icon used for users, projects, knowledge models and tenants.

An image when there is one, initials over a generated color when there is not.
The color is derived from the name, so the same item is always the same color.
An image that may turn out to be empty — transparent — goes over the initials
instead (`withImageOver`), so they show through when it does.

The existing implementation generates it in HSL, which is not perceptually
uniform: a yellow and a blue at the same HSL lightness look very different in
weight, so a row of chips reads as uneven. This derives in OKLCH at a fixed
perceptual lightness instead, which makes every hue sit at the same visual
weight — and, because lightness is fixed, contrast with the initials is
guaranteed for all 360 hues rather than being true by luck.

-}

import Color exposing (Color)
import Html exposing (Html, img, span, text)
import Html.Attributes exposing (alt, attribute, class, classList, src, style)
import Maybe.Extra as Maybe
import Ui.Icon as Icon exposing (Icon)
import Ui.Theme.Derive as Derive
import Ui.Theme.Oklch as Oklch
import Ui.Theme.Palette as Palette


type Size
    = Small
    | Medium
    | Large


type ItemIcon
    = ItemIcon
        { name : String
        , image : Maybe String
        , imageOver : Maybe String
        , size : Size
        , overrideInitials : Maybe String
        , glyph : Maybe Icon
        , hue : Maybe Float
        , square : Bool
        , plain : Bool
        , color : Maybe Color
        , brandOf : Maybe Color
        }


view : String -> ItemIcon
view name =
    ItemIcon { name = name, image = Nothing, imageOver = Nothing, size = Medium, overrideInitials = Nothing, glyph = Nothing, hue = Nothing, square = False, plain = False, color = Nothing, brandOf = Nothing }


{-| For an item whose displayed initials are not simply the start of its name —
a user with a separate first and last name, say.
-}
custom : { name : String, initials : String } -> ItemIcon
custom config =
    ItemIcon
        { name = config.name
        , image = Nothing
        , imageOver = Nothing
        , size = Medium
        , overrideInitials = Just config.initials
        , glyph = Nothing
        , hue = Nothing
        , square = False
        , plain = False
        , color = Nothing
        , brandOf = Nothing
        }


{-| A glyph in the circle instead of initials, for an item whose kind says more
than its name's first letter — a file, whose name begins with whatever someone
called it and whose type is the thing worth seeing.

The ground is still derived from the name, so a caller that wants every file of
a kind to look alike gives the kind as the name.

-}
withGlyph : Icon -> ItemIcon -> ItemIcon
withGlyph icon (ItemIcon config) =
    ItemIcon { config | glyph = Just icon }


{-| A hue chosen by the caller rather than derived from the name, for icons that
have to be told apart side by side — the people in a project right now, whose
names ("Anonymous Otter", "Anonymous Owl") would land on neighboring hues. The
color is a little stronger than a derived one, for the same reason; the
lightness is the same, so the dark initials or glyph keep their contrast.
-}
withHue : Float -> ItemIcon -> ItemIcon
withHue hue (ItemIcon config) =
    ItemIcon { config | hue = Just hue }


withImage : Maybe String -> ItemIcon -> ItemIcon
withImage image (ItemIcon config) =
    ItemIcon { config | image = image }


{-| An image laid over the initials rather than in place of them, for one that
may have nothing in it: a picture service asked for a transparent image where it
has none. Where it is transparent the initials show; where it has a picture,
that covers them. `withImage` wins over it.
-}
withImageOver : Maybe String -> ItemIcon -> ItemIcon
withImageOver image (ItemIcon config) =
    ItemIcon { config | imageOver = image }


{-| A square with rounded corners instead of a circle, for a place rather than a
person or a thing in it — a workspace, an organization. An image is fitted into
the square whole, not cropped to it: what is uploaded for a place is a logo, and
a logo with its edges cut off is a different logo.
-}
withSquare : ItemIcon -> ItemIcon
withSquare (ItemIcon config) =
    ItemIcon { config | square = True }


{-| A quiet gray ground instead of a color derived from the name, for a glyph
that stands for an action or a destination rather than for an item — "All
workspaces" next to the workspaces themselves. It takes the same box as the
icons beside it, so the labels line up, without claiming to be one of them.
-}
withPlainGround : ItemIcon -> ItemIcon
withPlainGround (ItemIcon config) =
    ItemIcon { config | plain = True }


{-| The ground and the text of a primary button, for the brand a seed color
gives — whatever the theme around it is. For a glyph that belongs to one brand
and is shown inside another's theme: the organization's "Home" while the reader
is in a workspace with a color of its own.

It is derived as the theme derives a filled button's ground (`Ui.Theme.Derive`),
for both modes, and the stylesheet picks the one in force; the text on it is the
theme's own text-on-brand, which does not depend on the seed.

-}
withBrandOf : Color -> ItemIcon -> ItemIcon
withBrandOf seed (ItemIcon config) =
    ItemIcon { config | brandOf = Just seed }


{-| The item's own color as the ground, for an item that has one — a workspace
with a primary color — instead of one derived from its name. The initials or
the glyph take whichever of white and near-black reads better against it, so
any color somebody picks stays legible.
-}
withColor : Color -> ItemIcon -> ItemIcon
withColor color (ItemIcon config) =
    ItemIcon { config | color = Just color }


withSize : Size -> ItemIcon -> ItemIcon
withSize size (ItemIcon config) =
    ItemIcon { config | size = size }


toHtml : ItemIcon -> Html msg
toHtml (ItemIcon config) =
    let
        classes =
            classList
                [ ( "ui-item-icon", True )
                , ( "ui-item-icon--sm", config.size == Small )
                , ( "ui-item-icon--lg", config.size == Large )
                , ( "ui-item-icon--square", config.square )
                , ( "ui-item-icon--plain", config.plain )
                , ( "ui-item-icon--brand", Maybe.isJust config.brandOf )
                ]
    in
    -- A span rather than a div, and a flex box by its stylesheet: the same
    -- picture either way, and this one is also allowed inside a button — which
    -- is what a combobox's chosen value sits in.
    case config.image of
        Just url ->
            span [ classes ]
                -- The name is carried by the surrounding text in every current
                -- use, so the image itself is decorative.
                [ img [ src url, alt "" ] [] ]

        Nothing ->
            span
                (classes
                    :: attribute "aria-hidden" "true"
                    :: (if config.plain then
                            []

                        else if Maybe.isJust config.brandOf then
                            case config.brandOf of
                                Just seed ->
                                    [ style "--ui-item-icon-brand-light" (Palette.toHex (Derive.lightRoles Palette.lightConstraints seed).strong)
                                    , style "--ui-item-icon-brand-dark" (Palette.toHex (Derive.darkRoles Palette.darkConstraints seed).strong)
                                    ]

                                Nothing ->
                                    []

                        else if Maybe.isJust config.color then
                            case config.color of
                                Just color ->
                                    [ style "background-color" (Palette.toHex color) ]

                                Nothing ->
                                    []

                        else
                            [ style "background-color"
                                (case config.hue of
                                    Just hue ->
                                        Palette.toHex (Oklch.toColor { l = 0.88, c = 0.09, h = hue })

                                    Nothing ->
                                        background config.name
                                )
                            ]
                       )
                )
                ((case config.glyph of
                    Just icon ->
                        span (class "ui-item-icon__glyph" :: onColor config.color) [ Icon.toHtml icon ]

                    Nothing ->
                        span (class "ui-item-icon__initials" :: onColor config.color) [ text (initials config) ]
                 )
                    :: (case config.imageOver of
                            Just url ->
                                [ img [ class "ui-item-icon__over", src url, alt "" ] [] ]

                            Nothing ->
                                []
                       )
                )


{-| White or near-black on the item's own color, whichever has the higher
contrast with it. Nothing to say on a derived ground: the stylesheet's dark
initials are right for every one of those.
-}
onColor : Maybe Color -> List (Html.Attribute msg)
onColor mbColor =
    case mbColor of
        Just color ->
            let
                dark =
                    Color.rgb255 17 24 39
            in
            if Oklch.contrast Color.white color >= Oklch.contrast dark color then
                [ style "color" "#ffffff" ]

            else
                [ style "color" (Palette.toHex dark) ]

        Nothing ->
            []


initials : { a | name : String, overrideInitials : Maybe String } -> String
initials config =
    case config.overrideInitials of
        Just value ->
            String.toUpper (String.left 2 value)

        -- The name's own first letter, as it is written: an item's name is not
        -- a person's, and "dsw:core" or "mDMP" is not abbreviated to a capital.
        Nothing ->
            String.left 1 config.name


{-| A stable hue per name. The sum of character codes is what the existing
implementation uses and it is good enough — the aim is only that the same name
always lands on the same color.
-}
background : String -> String
background name =
    let
        hue =
            name
                |> String.toList
                |> List.map Char.toCode
                |> List.sum
                |> modBy 360
                |> toFloat
    in
    -- Lightness 0.88 keeps the initials above 10:1 for every hue; the dark-mode
    -- counterpart is applied in CSS via a filter-free override.
    Palette.toHex (Oklch.toColor { l = 0.88, c = 0.06, h = hue })
