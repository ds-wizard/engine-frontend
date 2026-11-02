module TestUtils exposing
    ( parametrized
    , parametrizedWithTitle
    )

import Expect exposing (Expectation)
import String exposing (fromInt)
import Test exposing (Test, describe, test)


parametrized : List a -> String -> (a -> Expectation) -> Test
parametrized fixtures desc testFunction =
    describe desc
        (List.indexedMap (\i f -> test (desc ++ " " ++ fromInt i) (\_ -> testFunction f)) fixtures)


parametrizedWithTitle : List a -> String -> (a -> String) -> (a -> Expectation) -> Test
parametrizedWithTitle fixtures desc toTestTitle testFunction =
    describe desc
        (List.map (\f -> test (desc ++ " " ++ toTestTitle f) (\_ -> testFunction f)) fixtures)
