module TestUtils exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


parametrized : List a -> String -> (a -> Expectation) -> Test
parametrized fixtures desc testFunction =
    describe desc
        (List.map (\f -> test (desc ++ " " ++ toString f) (\_ -> testFunction f)) fixtures)
