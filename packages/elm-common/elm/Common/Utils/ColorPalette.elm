module Common.Utils.ColorPalette exposing (accents, all)

{-| The colors a color field offers under one press.

The wizard offers twenty, which are ten pairs of the same hue a shade apart —
two teals, two greens, two blues. A palette is for choosing quickly, and a
choice between two colors that look alike at a glance is not a quick one, so
this is one of each: ten hues that can be told apart across a list of tags.
Anything else is still reachable through the picker and the hex.

-}


{-| All ten, for a color that only tells things apart, such as a tag's.
-}
all : List String
all =
    [ "#16A085", "#27AE60", "#2980B9", "#8E44AD", "#2C3E50", "#F1C40F", "#E67E22", "#C0392B", "#7F8C8D", "#BDC3C7" ]


{-| Without the near-black and the grays, for a color the theme takes its
accents from: an accent in one of those is not an accent.
-}
accents : List String
accents =
    [ "#16A085", "#27AE60", "#2980B9", "#8E44AD", "#F1C40F", "#E67E22", "#C0392B" ]
