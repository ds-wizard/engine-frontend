module UuidTest exposing (suite)

import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Test exposing (Test, describe, test)
import Uuid


suite : Test
suite =
    describe "Uuid"
        [ describe "fromString"
            [ accepts "version 1" "c232ab00-9414-11ec-b3c8-9f6bdeced846"
            , accepts "version 4" "919108f7-52d1-4320-9bac-f847db4148a8"
            , accepts "version 6" "1ec9414c-232a-6b00-b3c8-9f6bdeced846"
            , accepts "version 7" "017f22e2-79b0-7cc3-98c4-dc0c0c07398f"
            , accepts "version 8" "2489e9ad-2ee2-8e00-8ec9-32d5f69181c0"
            , accepts "the nil uuid" "00000000-0000-0000-0000-000000000000"
            , test "lowercases what it accepts" <|
                \_ ->
                    Uuid.fromString "017F22E2-79B0-7CC3-98C4-DC0C0C07398F"
                        |> Maybe.map Uuid.toString
                        |> Expect.equal (Just "017f22e2-79b0-7cc3-98c4-dc0c0c07398f")
            , refuses "version 0" "017f22e2-79b0-0cc3-98c4-dc0c0c07398f"
            , refuses "version 9" "017f22e2-79b0-9cc3-98c4-dc0c0c07398f"
            , refuses "a variant other than RFC 9562's" "017f22e2-79b0-7cc3-c8c4-dc0c0c07398f"
            , refuses "a short one" "017f22e2-79b0-7cc3-98c4-dc0c0c07398"
            , refuses "one that is not hexadecimal" "017f22e2-79b0-7cc3-98c4-dc0c0c07398g"
            , refuses "one without its dashes" "017f22e279b07cc398c4dc0c0c07398f"
            ]
        , test "the decoder takes a version 7 uuid, as the server sends it" <|
            \_ ->
                Encode.string "017f22e2-79b0-7cc3-98c4-dc0c0c07398f"
                    |> Decode.decodeValue Uuid.decoder
                    |> Result.map Uuid.toString
                    |> Expect.equal (Ok "017f22e2-79b0-7cc3-98c4-dc0c0c07398f")
        , test "a generated uuid is one it accepts" <|
            \_ ->
                Random.step Uuid.uuidGenerator (Random.initialSeed 42)
                    |> Tuple.first
                    |> Uuid.toString
                    |> Uuid.fromString
                    |> Expect.notEqual Nothing
        ]


accepts : String -> String -> Test
accepts what value =
    test ("accepts " ++ what) <|
        \_ ->
            Uuid.fromString value
                |> Maybe.map Uuid.toString
                |> Expect.equal (Just value)


refuses : String -> String -> Test
refuses what value =
    test ("refuses " ++ what) <|
        \_ ->
            Uuid.fromString value
                |> Expect.equal Nothing
