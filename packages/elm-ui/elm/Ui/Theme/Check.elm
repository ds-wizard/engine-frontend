module Ui.Theme.Check exposing
    ( Sheet
    , StatusGroup
    , Violation
    , violations
    )

{-| One pure function over a resolved sheet, used in three places: elm-test
against adversarial seeds, the admin at save time, and the styleguide's live
readout. Built once, so all three are affordable.

Every foreground role is checked against the _extreme_ surface it can appear on,
not the page ground. In light mode that is the sunken surface; in dark mode it
is the overlay, which is the lightest dark surface rather than the darkest.

-}

import Color exposing (Color)
import Maybe.Extra as Maybe
import Ui.Theme.Oklch as Oklch


type alias Sheet =
    { surface : Color
    , surfaceFrame : Color
    , surfaceSunken : Color

    -- The three grounds a raised thing paints. All white in light mode, where a
    -- shadow and a hairline do the lifting; three steps in dark, where they
    -- cannot. Checked separately because text sits on all of them.
    , surfaceElevated1 : Color
    , surfaceElevated2 : Color
    , surfaceElevated3 : Color

    -- The furthest a state layer can push a surface: the darkest ground in
    -- light mode, the lightest in dark. Text sits on this whenever a row is
    -- hovered or pressed, so it belongs in the check.
    , surfaceStateExtreme : Color
    , text : Color
    , textMuted : Color
    , textSubtle : Color
    , textOnBrand : Color
    , link : Color
    , linkHover : Color
    , borderControl : Color
    , focusRing : Color
    , brandStrong : Color
    , brandHover : Color
    , brandActive : Color
    , brandSubtle : Color
    , brandSubtleSelected : Color
    , brandOnSubtle : Color
    , statuses : List StatusGroup

    -- Syntax highlighting, checked against the two grounds code sits on rather
    -- than against every surface: a keyword never appears on a menu.
    , syntax : List ( String, Color )

    -- Chart series, at the non-text floor: a series is a meaningful graphic
    -- rather than text, and it sits on the page or in a card.
    , chart : List ( String, Color )

    -- The value scale, `value-0` to `value-100`: the fill of a bar colored by
    -- how well something scores. A fill is a meaningful graphic, so it has to
    -- stand out from the track it sits in (surface-sunken) at 3:1 — either on
    -- its own, or through an edge drawn round every fill. With an edge, the
    -- edge is what the eye separates from the track and it is what is checked;
    -- without one, every step is.
    , valueScale : List ( String, Color )
    , valueEdge : Maybe Color
    }


type alias StatusGroup =
    { name : String
    , base : Color
    , on : Color
    , subtle : Color
    , border : Color
    , onSubtle : Color
    }


type alias Violation =
    { pairing : String
    , ratio : Float
    , required : Float
    }


{-| Empty means the sheet conforms.
-}
violations : Sheet -> List Violation
violations sheet =
    let
        surfaces =
            [ ( "surface", sheet.surface )
            , ( "frame", sheet.surfaceFrame )
            , ( "surface-sunken", sheet.surfaceSunken )
            , ( "elevation-1", sheet.surfaceElevated1 )
            , ( "elevation-2", sheet.surfaceElevated2 )
            , ( "elevation-3", sheet.surfaceElevated3 )
            , ( "surface under a state layer", sheet.surfaceStateExtreme )
            ]

        onEverySurface label color required =
            List.map
                (\( surfaceName, surfaceColor ) ->
                    check (label ++ " on " ++ surfaceName) color surfaceColor required
                )
                surfaces

        statusChecks group =
            [ check (group.name ++ "-on on " ++ group.name ++ "-base") group.on group.base 4.5
            , check (group.name ++ "-border on " ++ group.name ++ "-subtle") group.border group.subtle 3.0
            , check (group.name ++ "-on-subtle on " ++ group.name ++ "-subtle") group.onSubtle group.subtle 4.5
            , check (group.name ++ "-base on surface") group.base sheet.surface 3.0

            -- A status color is also used as plain text on a neutral surface —
            -- a danger menu item, most visibly — so it has to survive a state
            -- layer there as well as sitting on its own tint.
            , check (group.name ++ "-on-subtle on a surface under a state layer")
                group.onSubtle
                sheet.surfaceStateExtreme
                4.5
            ]

        chartChecks ( name, color ) =
            [ check (name ++ " on surface") color sheet.surface 3.0
            , check (name ++ " on surface-sunken") color sheet.surfaceSunken 3.0
            ]

        valueChecks =
            case sheet.valueEdge of
                Just edge ->
                    [ check "value-edge on the track (surface-sunken)" edge sheet.surfaceSunken 3.0 ]

                Nothing ->
                    List.map
                        (\( name, color ) -> check (name ++ " on the track (surface-sunken)") color sheet.surfaceSunken 3.0)
                        sheet.valueScale

        -- Code sits on the page or in a well, and nowhere else.
        syntaxChecks ( name, color ) =
            [ check (name ++ " on surface") color sheet.surface 4.5
            , check (name ++ " on surface-sunken") color sheet.surfaceSunken 4.5
            ]
    in
    List.concat
        [ onEverySurface "text" sheet.text 4.5
        , onEverySurface "text-muted" sheet.textMuted 4.5
        , onEverySurface "text-subtle" sheet.textSubtle 4.5
        , onEverySurface "link" sheet.link 4.5
        , onEverySurface "link-hover" sheet.linkHover 4.5
        , onEverySurface "border-control" sheet.borderControl 3.0
        , onEverySurface "focus-ring" sheet.focusRing 3.0
        , [ check "text-on-brand on brand-strong" sheet.textOnBrand sheet.brandStrong 4.5
          , check "text-on-brand on brand-hover" sheet.textOnBrand sheet.brandHover 4.5
          , check "text-on-brand on brand-active" sheet.textOnBrand sheet.brandActive 4.5
          , check "text on brand-subtle" sheet.text sheet.brandSubtle 4.5
          , check "border-control on brand-subtle" sheet.borderControl sheet.brandSubtle 3.0

          -- A tinted ground is where the link color ends up: an active
          -- navigation item is link-colored text on brand-subtle, and the
          -- selected one on the stronger tint. Neither pairing was checked
          -- until the two-tint navigation group needed the second.
          , check "link on brand-subtle" sheet.link sheet.brandSubtle 4.5
          , check "text on brand-subtle-selected" sheet.text sheet.brandSubtleSelected 4.5
          , check "brand-on-subtle on brand-subtle" sheet.brandOnSubtle sheet.brandSubtle 4.5
          , check "brand-on-subtle on brand-subtle-selected" sheet.brandOnSubtle sheet.brandSubtleSelected 4.5
          ]
        , List.concatMap statusChecks sheet.statuses
        , List.concatMap syntaxChecks sheet.syntax
        , List.concatMap chartChecks sheet.chart
        , valueChecks
        ]
        |> Maybe.values


check : String -> Color -> Color -> Float -> Maybe Violation
check pairing foreground background required =
    let
        ratio =
            Oklch.contrast foreground background
    in
    if ratio >= required then
        Nothing

    else
        Just { pairing = pairing, ratio = ratio, required = required }
