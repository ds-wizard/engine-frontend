module Ui.DeriveTest exposing (suite)

import Color exposing (Color)
import Expect
import Test exposing (Test, describe, test)
import Ui.Theme.Derive as Derive
import Ui.Theme.Oklch as Oklch
import Ui.Theme.Palette as Palette


surfaceSunken : Color
surfaceSunken =
    Color.rgb255 0xEB 0xEF 0xF2


surfaceOverlay : Color
surfaceOverlay =
    Color.rgb255 0x2A 0x2E 0x33


toHex : Color -> String
toHex color =
    let
        rgba =
            Color.toRgba color

        part value =
            let
                n =
                    round (value * 255)

                digits =
                    "0123456789ABCDEF"

                at i =
                    String.slice i (i + 1) digits
            in
            at (n // 16) ++ at (modBy 16 n)
    in
    "#" ++ part rgba.red ++ part rgba.green ++ part rgba.blue


suite : Test
suite =
    describe "Ui.Theme.Derive"
        [ test "light brand-strong clears AA against the darkest light surface" <|
            \_ ->
                let
                    roles =
                        Derive.lightRoles Palette.lightConstraints Derive.defaultSeed
                in
                Oklch.contrast roles.strong surfaceSunken
                    |> Expect.atLeast 4.5
        , test "white text clears AA on light brand-strong" <|
            \_ ->
                let
                    roles =
                        Derive.lightRoles Palette.lightConstraints Derive.defaultSeed
                in
                Oklch.contrast roles.strong Color.white
                    |> Expect.atLeast 4.5
        , test "dark brand clears AA against the lightest dark surface" <|
            \_ ->
                let
                    roles =
                        Derive.darkRoles Palette.darkConstraints Derive.defaultSeed
                in
                Oklch.contrast roles.strong surfaceOverlay
                    |> Expect.atLeast 4.5
        , test "derivation holds the seed hue within one degree" <|
            \_ ->
                let
                    roles =
                        Derive.lightRoles Palette.lightConstraints Derive.defaultSeed

                    seedHue =
                        (Oklch.fromColor Derive.defaultSeed).h

                    strongHue =
                        (Oklch.fromColor roles.strong).h
                in
                abs (seedHue - strongHue)
                    |> Expect.lessThan 1.0
        , test "the seed itself is kept as the identity role" <|
            \_ ->
                Derive.lightRoles Palette.lightConstraints Derive.defaultSeed
                    |> .base
                    |> toHex
                    |> Expect.equal "#2563EB"
        ]
