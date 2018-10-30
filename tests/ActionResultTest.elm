module ActionResultTest exposing (..)

import ActionResult exposing (ActionResult(..))
import Expect exposing (Expectation)
import Test exposing (..)
import TestUtils exposing (parametrized)


isUnsetTests : Test
isUnsetTests =
    describe "isUnset"
        [ parametrized
            [ ( Unset, True )
            , ( Loading, False )
            , ( Success 2, False )
            , ( Error "err", False )
            ]
            "should work"
          <|
            \( ar, expected ) ->
                Expect.equal (ActionResult.isUnset ar) expected
        ]


isLoadingTests : Test
isLoadingTests =
    describe "isLoading"
        [ parametrized
            [ ( Unset, False )
            , ( Loading, True )
            , ( Success 2, False )
            , ( Error "err", False )
            ]
            "should work"
          <|
            \( ar, expected ) ->
                Expect.equal (ActionResult.isLoading ar) expected
        ]


isSuccessTests : Test
isSuccessTests =
    describe "isSuccess"
        [ parametrized
            [ ( Unset, False )
            , ( Loading, False )
            , ( Success 2, True )
            , ( Error "err", False )
            ]
            "should work"
          <|
            \( ar, expected ) ->
                Expect.equal (ActionResult.isSuccess ar) expected
        ]


isErrorTests : Test
isErrorTests =
    describe "isError"
        [ parametrized
            [ ( Unset, False )
            , ( Loading, False )
            , ( Success 2, False )
            , ( Error "err", True )
            ]
            "should work"
          <|
            \( ar, expected ) ->
                Expect.equal (ActionResult.isError ar) expected
        ]


mapTests : Test
mapTests =
    describe "map"
        [ test "should change value when success" <|
            \_ ->
                let
                    result =
                        ActionResult.map String.length <| Success "abcdefg"
                in
                Expect.equal result (Success 7)
        , test "should not change value when unset" <|
            \_ -> Expect.equal (ActionResult.map (always True) Unset) Unset
        , test "should not change value when loading" <|
            \_ -> Expect.equal (ActionResult.map (always True) Loading) Loading
        , test "should not change value when error" <|
            \_ -> Expect.equal (ActionResult.map (always True) (Error "err")) (Error "err")
        ]


withDefaultTests : Test
withDefaultTests =
    describe "withDefault"
        [ test "should return success value when success" <|
            \_ -> Expect.equal (ActionResult.withDefault "efg" (Success "abc")) "abc"
        , test "should return default value when unset" <|
            \_ -> Expect.equal (ActionResult.withDefault "efg" Unset) "efg"
        , test "should return default value when loading" <|
            \_ -> Expect.equal (ActionResult.withDefault 2 Loading) 2
        , test "should return default value when error" <|
            \_ -> Expect.equal (ActionResult.withDefault False (Error "err")) False
        ]


combineTests : Test
combineTests =
    describe "combine"
        [ parametrized
            [ ( Success "abc", Success "def", Success ( "abc", "def" ) )
            , ( Unset, Success "def", Unset )
            , ( Loading, Unset, Unset )
            , ( Loading, Error "err", Loading )
            , ( Success "abc", Loading, Loading )
            , ( Error "err", Success "def", Error "err" )
            , ( Success "abc", Error "err", Error "err" )
            ]
            "should work"
          <|
            \( ar1, ar2, expected ) ->
                Expect.equal (ActionResult.combine ar1 ar2) expected
        ]


combine3Tests : Test
combine3Tests =
    describe "combine3"
        [ parametrized
            [ ( Success 1, Success 2, Success 3, Success ( 1, 2, 3 ) )
            , ( Unset, Success 2, Loading, Unset )
            , ( Success 1, Loading, Success 2, Loading )
            , ( Success 1, Success 2, Error "err", Error "err" )
            ]
            "should work"
          <|
            \( ar1, ar2, ar3, expected ) ->
                Expect.equal (ActionResult.combine3 ar1 ar2 ar3) expected
        ]
