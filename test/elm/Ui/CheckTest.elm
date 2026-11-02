module Ui.CheckTest exposing (suite)

import Color exposing (Color)
import Expect
import Test exposing (Test, describe, test)
import Ui.Theme as Theme
import Ui.Theme.Check as Check
import Ui.Theme.Derive as Derive
import Ui.Theme.Oklch as Oklch
import Ui.Theme.Palette as Palette


{-| 24 hues around the circle at three chromas. This is the test that separates
a system which works for our blue from one that works for a tenant's.
-}
adversarialSeeds : List Color
adversarialSeeds =
    let
        hues =
            List.map (\i -> toFloat i * 15) (List.range 0 23)

        chromas =
            [ 0.04, 0.11, 0.2 ]
    in
    List.concatMap
        (\hue ->
            List.map
                (\chroma -> Oklch.toColor { l = 0.65, c = chroma, h = hue })
                chromas
        )
        hues


describeViolations : List Check.Violation -> String
describeViolations found =
    found
        |> List.map
            (\v ->
                v.pairing
                    ++ " "
                    ++ String.fromFloat (toFloat (round (v.ratio * 100)) / 100)
                    ++ ":1 < "
                    ++ String.fromFloat v.required
            )
        |> String.join "; "


expectNoViolations : Check.Sheet -> Expect.Expectation
expectNoViolations sheet =
    case Check.violations sheet of
        [] ->
            Expect.pass

        found ->
            Expect.fail (describeViolations found)


suite : Test
suite =
    describe "Ui.Theme.Check"
        [ test "the default light sheet conforms" <|
            \_ ->
                expectNoViolations (Theme.sheetFor Theme.Light Theme.default)
        , test "the default dark sheet conforms" <|
            \_ ->
                expectNoViolations (Theme.sheetFor Theme.Dark Theme.default)
        , test "every adversarial seed conforms in light mode" <|
            \_ ->
                adversarialSeeds
                    |> List.concatMap
                        (\seed ->
                            Derive.lightRoles Palette.lightConstraints seed
                                |> Palette.lightSheet
                                |> Check.violations
                        )
                    |> describeViolations
                    |> Expect.equal ""
        , test "every adversarial seed conforms in dark mode" <|
            \_ ->
                adversarialSeeds
                    |> List.concatMap
                        (\seed ->
                            Derive.darkRoles Palette.darkConstraints seed
                                |> Palette.darkSheet
                                |> Check.violations
                        )
                    |> describeViolations
                    |> Expect.equal ""
        ]
