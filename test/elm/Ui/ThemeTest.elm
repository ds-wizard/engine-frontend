module Ui.ThemeTest exposing (suite)

import Color
import Expect
import Test exposing (Test, describe, test)
import Ui.Theme as Theme


{-| These pin the _contract between Elm and the JS port_, which is where the
mode switch broke: the emitted CSS carries selectors, so it has to be injected
as a style element rather than set as a style attribute, and the scopes have to
target :root because that is what \_roles.scss keys on.
-}
suite : Test
suite =
    describe "Ui.Theme"
        [ test "emits a light scope targeting the root" <|
            \_ ->
                Theme.toStyleString Theme.default
                    |> String.contains ":root,:root[data-theme=\"light\"]{"
                    |> Expect.equal True
        , test "emits an explicit dark scope targeting the root" <|
            \_ ->
                Theme.toStyleString Theme.default
                    |> String.contains ":root[data-theme=\"dark\"]{"
                    |> Expect.equal True
        , test "emits a system dark scope that an explicit light choice beats" <|
            \_ ->
                Theme.toStyleString Theme.default
                    |> String.contains "@media (prefers-color-scheme: dark){:root:not([data-theme=\"light\"]){"
                    |> Expect.equal True
        , test "carries selectors, so it cannot be a style attribute" <|
            \_ ->
                -- A style attribute drops anything with a brace, silently,
                -- so the theme has to be a style element.
                Theme.toStyleString Theme.default
                    |> String.contains "{"
                    |> Expect.equal True
        , test "a changed seed changes the emitted brand roles" <|
            \_ ->
                let
                    other =
                        Theme.withSeed (Color.rgb255 200 40 40) Theme.default
                in
                Theme.toStyleString other
                    |> Expect.notEqual (Theme.toStyleString Theme.default)
        , test "mode maps to the attribute value the port stamps on :root" <|
            \_ ->
                [ Theme.modeToAttribute Theme.Light
                , Theme.modeToAttribute Theme.Dark
                , Theme.modeToAttribute Theme.System
                ]
                    |> Expect.equal [ Just "light", Just "dark", Nothing ]
        ]
