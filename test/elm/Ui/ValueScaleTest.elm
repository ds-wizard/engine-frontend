module Ui.ValueScaleTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Ui.Theme as Theme
import Ui.Theme.Check as Check
import Ui.Theme.Palette as Palette
import Ui.ValueScale as ValueScale


{-| The steps as `_roles.scss` spells them. The stylesheet is what the browser
paints and the derivation is what the checker verifies, so the two have to be
the same colors; a stop moved in one place and not the other fails here.
-}
lightInStylesheet : List String
lightInStylesheet =
    [ "#e23532", "#ee540e", "#f17500", "#f49000", "#f7aa00", "#f8c20d", "#d4bc00", "#aeb600", "#86ae22", "#59a438", "#139948" ]


darkInStylesheet : List String
darkInStylesheet =
    [ "#f0574e", "#f76a38", "#fa7f1f", "#f99400", "#f6a800", "#efbd24", "#d4be1d", "#b7bf2a", "#96be3e", "#71bc53", "#43b966" ]


hexes : Theme.Mode -> List String
hexes mode =
    (Theme.sheetFor mode Theme.default).valueScale
        |> List.map (Tuple.second >> Palette.toHex)


suite : Test
suite =
    describe "Ui.ValueScale"
        [ test "a value is rounded to the nearest tenth, as the wizard did" <|
            \_ ->
                List.map ValueScale.step [ 0, 0.04, 0.05, 0.44, 0.45, 0.5, 0.94, 0.95, 1 ]
                    |> Expect.equal [ 0, 0, 10, 40, 50, 50, 90, 100, 100 ]
        , test "a value outside 0..1 is held at the end it passed" <|
            \_ ->
                List.map ValueScale.step [ -0.3, 1.7 ]
                    |> Expect.equal [ 0, 100 ]
        , test "the light steps are the stylesheet's" <|
            \_ ->
                hexes Theme.Light |> Expect.equal lightInStylesheet
        , test "the dark steps are the stylesheet's" <|
            \_ ->
                hexes Theme.Dark |> Expect.equal darkInStylesheet
        , test "the steps are named as the roles are" <|
            \_ ->
                (Theme.sheetFor Theme.Light Theme.default).valueScale
                    |> List.map Tuple.first
                    |> Expect.equal (List.map (\index -> "value-" ++ String.fromInt (index * 10)) (List.range 0 10))
        , test "without its edge the light scale would not stand out from the track" <|
            \_ ->
                -- Which is the whole reason the edge exists: the yellows are
                -- far short of 3:1 on the light track.
                Check.violations { lightSheet | valueEdge = Nothing }
                    |> List.isEmpty
                    |> Expect.equal False
        , test "the dark scale stands out from the track without an edge" <|
            \_ ->
                Theme.sheetFor Theme.Dark Theme.default
                    |> .valueEdge
                    |> Expect.equal Nothing
        ]


lightSheet : Check.Sheet
lightSheet =
    Theme.sheetFor Theme.Light Theme.default
