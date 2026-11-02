module CharIdentifierTest exposing (fromIntTests)

import CharIdentifier
import Expect
import Test exposing (Test, describe)
import TestUtils exposing (parametrized)


fromIntTests : Test
fromIntTests =
    describe "fromInt"
        [ parametrized
            [ ( 0, "a" )
            , ( 1, "b" )
            , ( 12, "m" )
            , ( 24, "y" )
            , ( 25, "z" )
            , ( 26, "aa" )
            , ( 27, "ab" )
            , ( 100, "cw" )
            , ( 708, "aag" )
            ]
            "should work"
          <|
            \( value, expected ) ->
                Expect.equal (CharIdentifier.fromInt value) expected
        ]
