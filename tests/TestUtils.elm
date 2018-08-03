module TestUtils exposing (..)

import Expect exposing (Expectation)
import Json.Decode exposing (Decoder, decodeString)
import Test exposing (Test, describe, test)


parametrized : List a -> String -> (a -> Expectation) -> Test
parametrized fixtures desc testFunction =
    describe desc
        (List.map (\f -> test (desc ++ " " ++ toString f) (\_ -> testFunction f)) fixtures)


expectDecoder : Decoder a -> String -> a -> Expectation
expectDecoder decoder raw expected =
    case decodeString decoder raw of
        Ok decoded ->
            Expect.equal decoded expected

        Err err ->
            Expect.fail err
