module Common.Plugins.IdentifierPatternTest exposing (suite)

import Common.Plugins.IdentifierPattern as IdentifierPattern
import Expect
import Test exposing (Test, describe, test)


matches : String -> String -> Bool
matches pattern identifier =
    IdentifierPattern.fromString pattern
        |> Maybe.map (\parsed -> IdentifierPattern.matches parsed identifier)
        |> Maybe.withDefault False


suite : Test
suite =
    describe "Common.Plugins.IdentifierPattern"
        [ describe "a pattern of an id and a version"
            [ test "matches the id in a version the pattern allows" <|
                \_ -> matches "dsw.root:^2" "dsw.root:2.4.0" |> Expect.equal True
            , test "does not match another version" <|
                \_ -> matches "dsw.root:^2" "dsw.root:3.0.0" |> Expect.equal False
            , test "does not match another id" <|
                \_ -> matches "dsw.root:^2" "dsw.other:2.4.0" |> Expect.equal False
            , test "matches any id with a star" <|
                \_ -> matches "*:^2" "anything:2.0.0" |> Expect.equal True
            ]
        , describe "a pattern from before ids were joined"
            [ test "names the id its two parts were joined into" <|
                \_ -> matches "dsw:root:^2" "dsw.root:2.4.0" |> Expect.equal True
            , test "does not match another knowledge model of the organization" <|
                \_ -> matches "dsw:root:^2" "dsw.other:2.4.0" |> Expect.equal False
            , test "matches every id of the organization with a star" <|
                \_ -> matches "dsw:*:^2" "dsw.other:2.4.0" |> Expect.equal True
            , test "and no id of another organization" <|
                \_ -> matches "dsw:*:^2" "dswx.other:2.4.0" |> Expect.equal False
            ]
        , describe "what is not a pattern or an identifier"
            [ test "a pattern without a version is not one" <|
                \_ -> IdentifierPattern.fromString "dsw.root" |> Expect.equal Nothing
            , test "an identifier in three parts matches nothing" <|
                \_ -> matches "dsw.root:^2" "dsw:root:2.4.0" |> Expect.equal False
            ]
        ]
