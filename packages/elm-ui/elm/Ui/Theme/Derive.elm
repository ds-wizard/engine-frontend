module Ui.Theme.Derive exposing
    ( BrandRoles
    , Constraints
    , darkRoles
    , defaultSeed
    , lightRoles
    , valueScale
    )

{-| Turns an arbitrary brand seed into a set of brand roles that clear the
contrast floors.

The seed is almost never usable as a text color. Most saturated mid-lightness
hues sit between 3:1 and 4.5:1 against white, so `strong` is derived by lowering
lightness until it clears AA against the _darkest_ surface the role can sit on —
not against white, which would pass on the page ground and fail inside a well.

-}

import Color exposing (Color)
import Ui.Theme.Oklch as Oklch


{-| What the derived roles have to satisfy. `subtle` is a background, so it can
only be placed once we know which foreground roles must remain legible on it —
which is why these are parameters rather than constants.
-}
type alias Constraints =
    { textSurface : Color
    , textOnSubtle : Color
    , borderOnSubtle : Color

    -- The furthest a hover or press moves the surface a link can sit on. Link
    -- is text and brand-strong is a background, so they answer to different
    -- grounds: a filled button carries its own color under it, a link does
    -- not.
    , linkSurface : Color
    }


type alias BrandRoles =
    { base : Color
    , strong : Color
    , hover : Color
    , active : Color
    , subtle : Color
    , subtleHover : Color

    -- One step past hover, for the thing that is chosen rather than pointed at.
    -- A navigation group becomes `subtle` while you are inside it and the page
    -- itself takes this, so the two read as the same color at two strengths —
    -- which is what the existing sidebar does with its 10% and 20% mixes.
    , subtleSelected : Color

    -- The brand as text *on* those tints. The four status groups have had an
    -- `on-subtle` role from the start and the brand did not, which held while
    -- the only tinted ground was the lightest one: the link color clears 4.5:1
    -- there. It does not clear it on the selected tint, so the foreground is
    -- derived against that one and used on both.
    , onSubtle : Color
    , link : Color
    , linkHover : Color
    }


{-| A plain blue that belongs to no product. An application with a brand of its
own passes its seed to `Ui.Theme.withSeed` and overrides the matching roles in
its stylesheet.
-}
defaultSeed : Color
defaultSeed =
    Color.rgb255 0x25 0x63 0xEB


{-| Light mode. `against` is the darkest surface a brand text role can sit on,
which in practice is `surface-sunken`.
-}
lightRoles : Constraints -> Color -> BrandRoles
lightRoles constraints seed =
    let
        base =
            Oklch.fromColor seed

        strongL =
            darkenUntil constraints.textSurface base 4.5

        toneAt lightness =
            Oklch.toColor (Oklch.withLightness lightness base)

        subtleBase =
            { base | c = base.c * 0.28 }

        subtleL =
            placeSubtle constraints subtleBase 0.955 0.004

        linkL =
            darkenUntil constraints.linkSurface base 4.5

        subtleSelectedColor =
            Oklch.toColor (Oklch.withLightness (subtleL - 0.06) { base | c = base.c * 0.45 })
    in
    { base = seed
    , strong = toneAt strongL
    , hover = toneAt (strongL - 0.055)
    , active = toneAt (strongL - 0.105)
    , subtle = Oklch.toColor (Oklch.withLightness subtleL subtleBase)
    , subtleHover = Oklch.toColor (Oklch.withLightness (subtleL - 0.03) { base | c = base.c * 0.34 })
    , subtleSelected = subtleSelectedColor
    , onSubtle = toneAt (darkenUntil subtleSelectedColor base 4.5)
    , link = toneAt linkL
    , linkHover = toneAt (linkL - 0.06)
    }


{-| Dark mode. `against` is the lightest dark surface a brand text role can sit
on, which is `surface-overlay` rather than the page ground.
-}
darkRoles : Constraints -> Color -> BrandRoles
darkRoles constraints seed =
    let
        base =
            Oklch.fromColor seed

        strongL =
            lightenUntil constraints.textSurface base 4.5

        toneAt lightness =
            Oklch.toColor (Oklch.withLightness lightness base)

        subtleBase =
            { base | c = base.c * 0.4 }

        subtleL =
            placeSubtle constraints subtleBase 0.31 -0.004

        linkL =
            lightenUntil constraints.linkSurface base 4.5

        subtleSelectedColor =
            Oklch.toColor (Oklch.withLightness (subtleL + 0.09) { base | c = base.c * 0.58 })
    in
    { base = toneAt strongL
    , strong = toneAt strongL
    , hover = toneAt (min 0.95 (strongL + 0.06))
    , active = toneAt (min 0.98 (strongL + 0.115))
    , subtle = Oklch.toColor (Oklch.withLightness subtleL subtleBase)
    , subtleHover = Oklch.toColor (Oklch.withLightness (subtleL + 0.05) { base | c = base.c * 0.44 })
    , subtleSelected = subtleSelectedColor
    , onSubtle = toneAt (lightenUntil subtleSelectedColor base 4.5)
    , link = toneAt linkL
    , linkHover = toneAt (min 0.95 (linkL + 0.06))
    }


{-| `subtle` is a tinted background, so it is placed by moving it away from the
foregrounds that must survive on it until both clear their floors. Light mode
steps upward, dark mode downward.
-}
placeSubtle : Constraints -> Oklch.Oklch -> Float -> Float -> Float
placeSubtle constraints base lightness step =
    if lightness <= 0.1 || lightness >= 0.995 then
        lightness

    else
        let
            candidate =
                Oklch.toColor (Oklch.withLightness lightness base)
        in
        if
            (Oklch.contrast candidate constraints.textOnSubtle >= 4.5)
                && (Oklch.contrast candidate constraints.borderOnSubtle >= 3.0)
        then
            lightness

        else
            placeSubtle constraints base (lightness + step) step


{-| Walk lightness down from the seed until the color clears `target` against
`against`. Steps of 0.005 keep the result close to the seed.
-}
darkenUntil : Color -> Oklch.Oklch -> Float -> Float
darkenUntil against base target =
    search against base target base.l -0.005


lightenUntil : Color -> Oklch.Oklch -> Float -> Float
lightenUntil against base target =
    search against base target base.l 0.005


search : Color -> Oklch.Oklch -> Float -> Float -> Float -> Float
search against base target lightness step =
    if lightness <= 0.12 || lightness >= 0.98 then
        lightness

    else if Oklch.contrast (Oklch.toColor (Oklch.withLightness lightness base)) against >= target then
        lightness

    else
        search against base target (lightness + step) step


{-| The value scale: eleven steps for a value from 0 to 1, from `from` at 0
through `via` at 0.5 to `to` at 1 — red, yellow, green, for how well something
scores.

Interpolated in OKLCH rather than mixed in sRGB. Lightness, chroma and hue each
move in equal steps along the two halves, so neighboring steps are the same
perceptual distance apart; an sRGB mix of red and green runs through a muddy
brown in the middle, and its steps bunch up where the eye separates them least.
A step the gamut cannot hold gives up chroma, never hue or lightness, as every
other derived color does.

-}
valueScale : { from : Oklch.Oklch, via : Oklch.Oklch, to : Oklch.Oklch } -> List Color
valueScale stops =
    let
        mix a b t =
            a + (b - a) * t

        between a b t =
            Oklch.toColor { l = mix a.l b.l t, c = mix a.c b.c t, h = mix a.h b.h t }

        stepAt index =
            if index <= 5 then
                between stops.from stops.via (toFloat index / 5)

            else
                between stops.via stops.to (toFloat (index - 5) / 5)
    in
    List.map stepAt (List.range 0 10)
