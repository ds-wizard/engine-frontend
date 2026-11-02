module Ui.ProgressBar exposing
    ( Tone(..)
    , Words
    , toHtml
    , toHtmlWithFigure
    , toHtmlWithWords
    , towardsLimit
    , withTone
    )

{-| How far along something is.

A `progressbar` with its value on it, because a filled box is not something a
screen reader reads. The number it reports is the real one — `4 of 10`, not a
percentage — so it is announced the way the figure beside it is written.

The tone is the caller's, and that is deliberate. Filling up is bad when it is a
quota and good when it is a questionnaire, and a component cannot tell which it
is looking at: `towardsLimit` is the quota rule the usage tables want, and
anything else says what it means. `ByValue` is the one tone that is not a
state: it colors the fill by how full the bar is, on the value scale
(`Ui.ValueScale`) — red empty, yellow half, green full — for a score, where more
is better by degrees rather than past a threshold.

-}

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (attribute, class, classList, style)
import Maybe.Extra as Maybe
import Ui.ValueScale as ValueScale


type Bar
    = Bar
        { label : String
        , value : Float
        , max : Float
        , tone : Tone
        , valueText : Float -> Float -> String
        , figure : Maybe String
        }


{-| `Success`, `Warning` and `Danger` are states, and mean what they say.
`ByValue` is not a state: the fill takes the value scale's color for the
fraction of the bar that is filled. The color is not the value, so write the
figure out beside the bar, as for any other tone.
-}
type Tone
    = Brand
    | Success
    | Warning
    | Danger
    | ByValue


{-| `label` is what is filling up — "Projects used", "Questions answered". It
names the bar, so it is read before the number rather than instead of it.
-}
toHtml : { label : String, value : Float, max : Float } -> Html msg
toHtml config =
    view (Bar { label = config.label, value = config.value, max = config.max, tone = Brand, valueText = english.value, figure = Nothing })


withTone : Tone -> { label : String, value : Float, max : Float } -> Html msg
withTone tone config =
    view (Bar { label = config.label, value = config.value, max = config.max, tone = tone, valueText = english.value, figure = Nothing })


{-| How the value is said: `value` is given the value and the maximum and
returns the figure as it would be read out — "4 of 10". `toHtml` and `withTone`
say it in English.
-}
type alias Words =
    { value : Float -> Float -> String
    }


english : Words
english =
    { value = \value max -> String.fromFloat value ++ " of " ++ String.fromFloat max }


{-| A bar with its tone and its words given.
-}
toHtmlWithWords : Words -> { label : String, value : Float, max : Float, tone : Tone } -> Html msg
toHtmlWithWords words config =
    view
        (Bar
            { label = config.label
            , value = config.value
            , max = config.max
            , tone = config.tone
            , valueText = words.value
            , figure = Nothing
            }
        )


{-| `toHtmlWithWords`, taller, with the figure written on the bar rather than
beside it, for a cell of a table where there is no room beside it: `4/10`, or
`3/5 (60%)` — the caller's, as it is to be read. The fill is a pale tint of its
tone, so the figure reads on it as on the track.
-}
toHtmlWithFigure : Words -> { label : String, value : Float, max : Float, tone : Tone, figure : String } -> Html msg
toHtmlWithFigure words config =
    view
        (Bar
            { label = config.label
            , value = config.value
            , max = config.max
            , tone = config.tone
            , valueText = words.value
            , figure = Just config.figure
            }
        )


{-| The rule the usage tables use: fine until 80% of the limit, a warning up to
it, and past it a problem. Kept here because all three of them had their own copy
of the thresholds.
-}
towardsLimit : { value : Float, max : Float } -> Tone
towardsLimit config =
    if config.max <= 0 then
        Brand

    else if config.value >= config.max then
        Danger

    else if config.value / config.max >= 0.8 then
        Warning

    else
        Brand


view : Bar -> Html msg
view (Bar config) =
    let
        -- A bar that is more than full is still a full bar; a negative one is
        -- empty. Either beats a stripe hanging out of its track.
        filled =
            if config.max <= 0 then
                0

            else
                clamp 0 1 (config.value / config.max)
    in
    div
        [ classList [ ( "ui-progress", True ), ( "ui-progress--figure", Maybe.isJust config.figure ) ]
        , attribute "role" "progressbar"
        , attribute "aria-label" config.label
        , attribute "aria-valuenow" (String.fromFloat config.value)
        , attribute "aria-valuemin" "0"
        , attribute "aria-valuemax" (String.fromFloat config.max)

        -- The figure as it would be said, because "60" on its own is not an
        -- answer to "how far along".
        , attribute "aria-valuetext" (config.valueText config.value config.max)
        ]
        [ span
            ([ classList
                [ ( "ui-progress__fill", True )
                , ( toneClass config.tone, True )
                ]
             , style "inline-size" (String.fromFloat (filled * 100) ++ "%")
             ]
                ++ (if config.tone == ByValue then
                        [ ValueScale.tone filled ]

                    else
                        []
                   )
            )
            []
        , case config.figure of
            Just figure ->
                -- Visual: aria-valuetext already says it.
                span [ class "ui-progress__figure", attribute "aria-hidden" "true" ] [ text figure ]

            Nothing ->
                text ""
        ]


toneClass : Tone -> String
toneClass tone =
    case tone of
        Brand ->
            "is-brand"

        Success ->
            "is-success"

        Warning ->
            "is-warning"

        Danger ->
            "is-danger"

        ByValue ->
            "is-value"
