module Ui.AvatarGroup exposing
    ( Person
    , Words
    , person
    , toHtml
    , toHtmlWithWords
    , withGlyph
    , withHue
    , withImage
    , withImageOver
    )

{-| A row of faces: who is in a project, who is editing it now.

`Ui.ItemIcon` draws one. This is the other half of that idea — several of them
overlapping, and a count where they stop. `OnlineUser` does it today for the
people editing a questionnaire.

Two things it gets right that a row of avatars usually does not:

  - **Everybody is named.** Each face carries its own accessible name, and the
    overflow says how many are behind it rather than showing "+3" and nothing
    else. The group itself is a list, so entering it says how many there are.
  - **The tooltips are for the eyes only.** An avatar is not interactive and so
    not focusable, which means a tooltip on it can never be reached by keyboard.
    That is fine as long as the name is not _only_ in the tooltip — it is the
    avatar's label as well, so nothing is lost and nothing is a keyboard trap.

-}

import Html exposing (Html, li, span, text, ul)
import Html.Attributes exposing (attribute, class)
import Ui.Icon exposing (Icon)
import Ui.ItemIcon as ItemIcon
import Ui.Tooltip as Tooltip


type Person
    = Person { name : String, image : Maybe String, imageOver : Maybe String, glyph : Maybe Icon, hue : Maybe Float }


person : String -> Person
person name =
    Person { name = name, image = Nothing, imageOver = Nothing, glyph = Nothing, hue = Nothing }


{-| The face's color, given rather than derived from the name, so that the
people side by side are told apart; see `Ui.ItemIcon.withHue`.
-}
withHue : Float -> Person -> Person
withHue hue (Person config) =
    Person { config | hue = Just hue }


{-| An icon in place of initials — somebody anonymous, told apart by the animal
they were given. The name is still the face's label.
-}
withGlyph : Icon -> Person -> Person
withGlyph glyph (Person config) =
    Person { config | glyph = Just glyph }


withImage : String -> Person -> Person
withImage image (Person config) =
    Person { config | image = Just image }


{-| An image over the initials, which show where it is transparent; see
`Ui.ItemIcon.withImageOver`.
-}
withImageOver : String -> Person -> Person
withImageOver image (Person config) =
    Person { config | imageOver = Just image }


{-| `label` names the group — "3 people editing" — and `visible` is how many
faces to show before counting the rest.
-}
toHtml : { label : String, visible : Int, people : List Person } -> Html msg
toHtml =
    toHtmlWithWords { more = \count -> String.fromInt count ++ " more" }


{-| How the overflow is named: `more` is given the number of people behind it —
"3 more". `toHtml` says it in English.
-}
type alias Words =
    { more : Int -> String
    }


{-| `toHtml`, with the words given.
-}
toHtmlWithWords : Words -> { label : String, visible : Int, people : List Person } -> Html msg
toHtmlWithWords words config =
    let
        shown =
            List.take config.visible config.people

        hidden =
            List.drop config.visible config.people
    in
    ul
        [ class "ui-avatars"
        , attribute "aria-label" config.label
        ]
        (List.map viewPerson shown ++ viewOverflow words hidden)


viewPerson : Person -> Html msg
viewPerson (Person config) =
    li [ class "ui-avatars__item" ]
        [ Tooltip.view { label = config.name, placement = Tooltip.Below }
            (ItemIcon.toHtml
                (ItemIcon.view config.name
                    |> ItemIcon.withSize ItemIcon.Small
                    |> ItemIcon.withImage config.image
                    |> ItemIcon.withImageOver config.imageOver
                    |> (case config.glyph of
                            Just glyph ->
                                ItemIcon.withGlyph glyph

                            Nothing ->
                                identity
                       )
                    |> (case config.hue of
                            Just hue ->
                                ItemIcon.withHue hue

                            Nothing ->
                                identity
                       )
                )
            )
        ]


{-| The ones that did not fit, as a number and as their names.

"+3" alone leaves a screen reader with a plus and a three; the label says how
many, and the tooltip says who, for anyone who can see it.

-}
viewOverflow : Words -> List Person -> List (Html msg)
viewOverflow words hidden =
    if List.isEmpty hidden then
        []

    else
        let
            names =
                List.map (\(Person config) -> config.name) hidden
        in
        [ li [ class "ui-avatars__item" ]
            [ Tooltip.view { label = String.join ", " names, placement = Tooltip.Below }
                (span
                    [ class "ui-avatars__more"
                    , attribute "role" "img"
                    , attribute "aria-label" (words.more (List.length hidden))
                    ]
                    [ text ("+" ++ String.fromInt (List.length hidden)) ]
                )
            ]
        ]
