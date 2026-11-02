module Ui.OklchTest exposing (suite)

import Color
import Expect
import Test exposing (Test, describe, test)
import Ui.Theme.Oklch as Oklch


{-| Colors spread around the hue circle at a chroma that is safely in gamut,
so a round trip is testing the maths rather than the gamut mapping.
-}
sample : List { l : Float, c : Float, h : Float }
sample =
    List.map (\i -> { l = 0.6, c = 0.08, h = toFloat i * 30 }) (List.range 0 11)


suite : Test
suite =
    describe "Ui.Theme.Oklch"
        [ test "sRGB round trip preserves lightness" <|
            \_ ->
                sample
                    |> List.filter
                        (\oklch ->
                            abs ((Oklch.fromColor (Oklch.toColor oklch)).l - oklch.l) > 0.01
                        )
                    |> List.length
                    |> Expect.equal 0
        , test "sRGB round trip preserves hue" <|
            \_ ->
                sample
                    |> List.filter
                        (\oklch ->
                            abs ((Oklch.fromColor (Oklch.toColor oklch)).h - oklch.h) > 1.0
                        )
                    |> List.length
                    |> Expect.equal 0
        , test "gamut mapping holds the hue when chroma is out of range" <|
            \_ ->
                -- A chroma no hue can reach in sRGB, so every one of these is
                -- mapped. Clipping channels instead would shift the hue.
                List.map (\i -> { l = 0.5, c = 0.4, h = toFloat i * 30 }) (List.range 0 11)
                    |> List.filter
                        (\oklch ->
                            abs ((Oklch.fromColor (Oklch.toColor oklch)).h - oklch.h) > 1.0
                        )
                    |> List.length
                    |> Expect.equal 0
        , test "contrast is symmetric" <|
            \_ ->
                Oklch.contrast Color.white Color.black
                    |> Expect.within (Expect.Absolute 0.001) (Oklch.contrast Color.black Color.white)
        , test "white on black is 21:1" <|
            \_ ->
                Oklch.contrast Color.white Color.black
                    |> Expect.within (Expect.Absolute 0.01) 21
        , test "the brand seed fails AA against white, which is why brand-strong exists" <|
            \_ ->
                Oklch.contrast (Color.rgb255 1 154 214) Color.white
                    |> Expect.all
                        [ Expect.greaterThan 3.0
                        , Expect.lessThan 4.5
                        ]
        ]
