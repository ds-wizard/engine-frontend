module Ui.Tag exposing
    ( Tag
    , list
    , tag
    , toHtml
    , withColor
    , withRemove
    , withTestId
    )

{-| A label on something: a project's tags, a question's tags, a package's
keywords.

Not a `Ui.Badge`. A badge reports a state the application worked out — draft,
failed, 3 — and its color is part of what it says. A tag is a word somebody
chose, and the only thing it says is the word.

A set of tags is a `ul`, so a screen reader says how many there are before it
reads them, and each tag is an item rather than the next word of a run-on
sentence.

-}

import Html exposing (Html, button, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, style, type_)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe
import Ui.Icon as Icon
import Ui.TestId as TestId


type Tag msg
    = Tag
        { label : String
        , color : Maybe String
        , remove : Maybe { label : String, onRemove : msg }
        , testId : Maybe String
        }


tag : String -> Tag msg
tag label =
    Tag { label = label, color = Nothing, remove = Nothing, testId = Nothing }


{-| A button inside the tag that takes it away.

The label is not optional and is not "Remove": it is the whole accessible name
of an icon-only button, and a row of buttons all called "Remove" is a row a
screen reader cannot tell apart — "Remove tag biology" is. The words are the
caller's, like every string in the system.

What has focus disappears with the tag, so an interface that removes one should
put focus somewhere on purpose; `Ui.TagInput.focusInput` does that for the
field.

-}
withRemove : { label : String, onRemove : msg } -> Tag msg -> Tag msg
withRemove remove (Tag config) =
    Tag { config | remove = Just remove }


{-| The color a question tag was given, as a dot in front of the words.

A dot, not a fill. A tenant picks the color, so text on it would pass or fail
contrast depending on the choice; the words stay on the tag's own ground, where
the roles have already been checked, and the color is decoration beside them.
It is hidden from assistive technology for the same reason it is not the tag's
background: the name is the label, and "blue" was never part of it.

-}
withColor : String -> Tag msg -> Tag msg
withColor color (Tag config) =
    Tag { config | color = Just color }


withTestId : String -> Tag msg -> Tag msg
withTestId testId (Tag config) =
    Tag { config | testId = Just testId }


toHtml : Tag msg -> Html msg
toHtml (Tag config) =
    span
        (classList
            [ ( "ui-tag", True )
            , ( "ui-tag--removable", Maybe.isJust config.remove )
            ]
            :: TestId.attributes config.testId
        )
        [ case config.color of
            Just color ->
                span
                    [ class "ui-tag__swatch"
                    , style "background-color" color
                    , attribute "aria-hidden" "true"
                    ]
                    []

            Nothing ->
                text ""
        , span [ class "ui-tag__label" ] [ text config.label ]
        , case config.remove of
            Just remove ->
                button
                    [ type_ "button"
                    , class "ui-tag__remove"
                    , attribute "aria-label" remove.label
                    , onClick remove.onRemove
                    ]
                    [ Icon.toHtml (Icon.solid "fa-xmark") ]

            Nothing ->
                text ""
        ]


{-| Tags as a list. Nothing at all when there are none: "list, 0 items" is an
announcement about an absence nobody asked about.
-}
list : List (Tag msg) -> Html msg
list tags =
    if List.isEmpty tags then
        text ""

    else
        ul [ class "ui-tag-list" ]
            (List.map (\entry -> li [ class "ui-tag-list__item" ] [ toHtml entry ]) tags)
