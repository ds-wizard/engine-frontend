module Ui.Icon exposing
    ( Icon
    , brand
    , className
    , fromClass
    , labeled
    , regular
    , solid
    , spinner
    , toHtml
    )

{-| Icons are decorative by default: they render `aria-hidden`, because Font
Awesome's glyphs live in the private use area and screen readers announce them
as noise. An icon that carries meaning has to be given a label, which makes the
accessible case the explicit one.
-}

import Html exposing (Html, i)
import Html.Attributes exposing (attribute, class)
import Maybe.Extra as Maybe


type Icon
    = Icon
        { name : String
        , label : Maybe String
        , spin : Bool
        }


solid : String -> Icon
solid name =
    Icon { name = "fas " ++ name, label = Nothing, spin = False }


{-| The outline weight. Used where a pair of icons has to read as on and off —
an unchecked box beside a checked one — which is the one place the difference
carries meaning rather than style.
-}
regular : String -> Icon
regular name =
    Icon { name = "far " ++ name, label = Nothing, spin = False }


{-| A brand's own mark — `"fa-claude"`, `"fa-github"` — from the brands set,
which has only the one weight.
-}
brand : String -> Icon
brand name =
    Icon { name = "fab " ++ name, label = Nothing, spin = False }


{-| An icon named the way data names one: a Font Awesome class string such as
`"far fa-file-pdf"`, `"fas fa-code"` or `"fab fa-github"` — a document
template's format, a custom menu link, an identity provider.

The weight is kept, because it is part of the name: `fa-github` exists only
among the brands and `fa-file-pdf` looks different in regular and solid. Both
the short prefixes (`fas`, `far`, `fab`) and the long ones (`fa-solid`,
`fa-regular`, `fa-brands`) are understood; a string with no weight is solid,
which is what Font Awesome itself assumes.

Everything that is not a Font Awesome token is dropped, so a value from the API
cannot put any other class on the element, and a string with no icon in it at
all renders the solid question mark rather than an empty box.

-}
fromClass : String -> Icon
fromClass classes =
    Icon { name = normalizeClass classes, label = Nothing, spin = False }


{-| The classes an icon renders with — `"fab fa-github"`. For tests, and for
the rare place that has to hand an icon to something outside Elm.
-}
className : Icon -> String
className (Icon config) =
    config.name


normalizeClass : String -> String
normalizeClass classes =
    let
        tokens =
            String.words classes

        glyphs =
            List.filter (\token -> String.startsWith "fa-" token && Maybe.isNothing (weightOf token)) tokens
    in
    case glyphs of
        [] ->
            "fas fa-question"

        _ ->
            let
                -- The first weight given wins.
                weight =
                    List.head (List.filterMap weightOf tokens)
            in
            String.join " " (Maybe.withDefault "fas" weight :: glyphs)


weightOf : String -> Maybe String
weightOf token =
    case token of
        "fas" ->
            Just "fas"

        "fa-solid" ->
            Just "fas"

        "fa" ->
            Just "fas"

        "far" ->
            Just "far"

        "fa-regular" ->
            Just "far"

        "fab" ->
            Just "fab"

        "fa-brands" ->
            Just "fab"

        _ ->
            Nothing


labeled : String -> Icon -> Icon
labeled label (Icon config) =
    Icon { config | label = Just label }


spinner : Icon
spinner =
    Icon { name = "fas fa-circle-notch", label = Nothing, spin = True }


toHtml : Icon -> Html msg
toHtml (Icon config) =
    let
        accessibility =
            case config.label of
                Just label ->
                    [ attribute "role" "img", attribute "aria-label" label ]

                Nothing ->
                    [ attribute "aria-hidden" "true" ]

        spinClass =
            if config.spin then
                " ui-spinner"

            else
                ""
    in
    i (class ("ui-icon " ++ config.name ++ spinClass) :: accessibility) []
