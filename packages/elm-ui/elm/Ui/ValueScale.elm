module Ui.ValueScale exposing
    ( step
    , tone
    )

{-| A color for how well something scores: a value from 0 to 1 on eleven
steps, red at 0, yellow at 0.5, green at 1. A project's metrics are colored by
it, and so are the metrics an answer scores in a questionnaire.

The colors are roles, `--ui-value-0` to `--ui-value-100`, derived in OKLCH so
that the steps are evenly spaced to the eye, and redefined for dark mode like
every other role. Nothing names one directly: `step` picks it, and `tone` puts
it on an element.

**The color is never the only signal.** Red and green are the pair a color
blind reader is most likely to confuse, so whatever wears a step must say its
value some other way too — the number beside a bar, the answer's own text
(1.4.1). `Ui.ProgressBar.ByValue` is a bar colored by the scale; it carries its
value for assistive technology, and the page writes it out beside the bar.

-}

import Html exposing (Attribute)
import Html.Attributes exposing (class)


{-| The step for a value, as the role names it: 0, 10, … 100. The value is
rounded to the nearest tenth, as the wizard did, and a value outside 0..1 is
held at the end it passed.

    step 0.44 == 40

    step 0.45 == 50

    step 1.2 == 100

-}
step : Float -> Int
step value =
    if isNaN value then
        0

    else
        round (clamp 0 1 value * 10) * 10


{-| Put the step for a value on an element.

It sets `--ui-value-tone` for the element and everything inside it, and paints
nothing on its own: a component decides what the color is for — the fill of a
bar, the edge of a highlighted answer — by reading `var(--ui-value-tone)` in its
own stylesheet. Where the element is a fill that has to stand out from a light
ground, draw `--ui-value-edge` round it as `.ui-progress__fill.is-value` does;
on the light theme most steps are too light to do it alone.

-}
tone : Float -> Attribute msg
tone value =
    class ("ui-value-" ++ String.fromInt (step value))
