module TestUtils exposing
    ( expectDecoder
    , expectEncodeDecode
    , parametrized
    )

import Expect exposing (Expectation)
import Json.Decode exposing (Decoder, decodeString, errorToString)
import Json.Encode as Encode exposing (Value)
import String exposing (fromInt)
import Test exposing (Test, describe, test)


parametrized : List a -> String -> (a -> Expectation) -> Test
parametrized fixtures desc testFunction =
    describe desc
        (List.indexedMap (\i f -> test (desc ++ " " ++ fromInt i) (\_ -> testFunction f)) fixtures)


expectDecoder : Decoder a -> String -> a -> Expectation
expectDecoder decoder raw expected =
    case decodeString decoder raw of
        Ok decoded ->
            Expect.equal decoded expected

        Err err ->
            Expect.fail <| errorToString err


expectEncodeDecode : (a -> Value) -> Decoder a -> a -> Expectation
expectEncodeDecode encode decoder expected =
    let
        raw =
            Encode.encode 0 <| encode expected
    in
    expectDecoder decoder raw expected
