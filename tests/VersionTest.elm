module VersionTest exposing (gettersTest, nextVersionTests)

import Expect
import List.Extra as List
import Test exposing (Test, describe)
import TestUtils exposing (parametrized)
import Version exposing (Version)


versions : List Version
versions =
    [ Version.create 1 0 0
    , Version.create 0 1 0
    , Version.create 0 0 1
    , Version.create 1 2 3
    ]


majors : List Int
majors =
    [ 1, 0, 0, 1 ]


minors : List Int
minors =
    [ 0, 1, 0, 2 ]


patches : List Int
patches =
    [ 0, 0, 1, 3 ]


gettersTest : Test
gettersTest =
    describe "Getters"
        [ parametrized (List.zip versions majors)
            "getMajor"
          <|
            \( version, major ) -> Expect.equal major <| Version.getMajor version
        , parametrized (List.zip versions minors)
            "getMinor"
          <|
            \( version, minor ) -> Expect.equal minor <| Version.getMinor version
        , parametrized (List.zip versions patches)
            "getPatch"
          <|
            \( version, patch ) -> Expect.equal patch <| Version.getPatch version
        ]


nextVersionTests : Test
nextVersionTests =
    describe "Next Versions"
        [ parametrized
            [ Version.create 1 0 0
            , Version.create 1 1 0
            , Version.create 1 1 1
            ]
            "nextMajor"
          <|
            \version -> Expect.equal (Version.create 2 0 0) <| Version.nextMajor version
        , parametrized
            [ ( Version.create 1 0 0, Version.create 1 1 0 )
            , ( Version.create 1 1 0, Version.create 1 2 0 )
            , ( Version.create 1 1 1, Version.create 1 2 0 )
            ]
            "nextMinor"
          <|
            \( version, expected ) -> Expect.equal expected <| Version.nextMinor version
        , parametrized
            [ ( Version.create 1 0 0, Version.create 1 0 1 )
            , ( Version.create 1 1 0, Version.create 1 1 1 )
            , ( Version.create 1 1 1, Version.create 1 1 2 )
            ]
            "nextPatch"
          <|
            \( version, expected ) -> Expect.equal expected <| Version.nextPatch version
        ]
