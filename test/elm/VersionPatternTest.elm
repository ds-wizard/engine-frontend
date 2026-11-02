module VersionPatternTest exposing (matchesTest)

import Expect
import Test exposing (Test, describe)
import TestUtils exposing (parametrizedWithTitle)
import Version exposing (Version)
import VersionPattern


type alias TestCase =
    { versionPattern : String
    , version : Version
    , expectedMatch : Bool
    }


testCases : List TestCase
testCases =
    [ -- Exact
      { versionPattern = "1.2.3", version = Version.create 1 2 3, expectedMatch = True }
    , { versionPattern = "1.2.3", version = Version.create 1 2 4, expectedMatch = False }
    , { versionPattern = "1.2.3", version = Version.create 1 2 2, expectedMatch = False }
    , { versionPattern = "1.2.3", version = Version.create 1 3 0, expectedMatch = False }

    -- Wildcard Any
    , { versionPattern = "*", version = Version.create 0 0 0, expectedMatch = True }
    , { versionPattern = "x", version = Version.create 99 99 99, expectedMatch = True }
    , { versionPattern = "X", version = Version.create 1 2 3, expectedMatch = True }

    -- Wildcard AnyMinor (1.x)
    , { versionPattern = "1.x", version = Version.create 1 0 0, expectedMatch = True }
    , { versionPattern = "1.X", version = Version.create 1 0 0, expectedMatch = True }
    , { versionPattern = "1.*", version = Version.create 1 999 999, expectedMatch = True }
    , { versionPattern = "1.x", version = Version.create 2 0 0, expectedMatch = False }
    , { versionPattern = "1.x", version = Version.create 0 9 9, expectedMatch = False }

    -- Wildcard AnyPatch (1.2.x)
    , { versionPattern = "1.2.x", version = Version.create 1 2 0, expectedMatch = True }
    , { versionPattern = "1.2.X", version = Version.create 1 2 0, expectedMatch = True }
    , { versionPattern = "1.2.*", version = Version.create 1 2 999, expectedMatch = True }
    , { versionPattern = "1.2.x", version = Version.create 1 3 0, expectedMatch = False }
    , { versionPattern = "1.2.x", version = Version.create 2 2 0, expectedMatch = False }

    -- Tilde (~1.2.0 => >=1.2.0 <1.3.0)
    , { versionPattern = "~1.2.0", version = Version.create 1 2 0, expectedMatch = True }
    , { versionPattern = "~1.2.0", version = Version.create 1 2 5, expectedMatch = True }
    , { versionPattern = "~1.2.0", version = Version.create 1 2 999, expectedMatch = True }
    , { versionPattern = "~1.2.0", version = Version.create 1 3 0, expectedMatch = False }
    , { versionPattern = "~1.2.0", version = Version.create 1 1 999, expectedMatch = False }

    -- Caret (^1.2.3 => >=1.2.3 <2.0.0)
    , { versionPattern = "^1.2.3", version = Version.create 1 2 3, expectedMatch = True }
    , { versionPattern = "^1.2.3", version = Version.create 1 9 9, expectedMatch = True }
    , { versionPattern = "^1.2.3", version = Version.create 2 0 0, expectedMatch = False }
    , { versionPattern = "^1.2.3", version = Version.create 1 2 2, expectedMatch = False }

    -- Caret with 0-major semantics:
    -- ^0.2.3 => >=0.2.3 <0.3.0
    , { versionPattern = "^0.2.3", version = Version.create 0 2 3, expectedMatch = True }
    , { versionPattern = "^0.2.3", version = Version.create 0 2 99, expectedMatch = True }
    , { versionPattern = "^0.2.3", version = Version.create 0 3 0, expectedMatch = False }
    , { versionPattern = "^0.2.3", version = Version.create 0 2 2, expectedMatch = False }

    -- ^0.0.3 => >=0.0.3 <0.0.4
    , { versionPattern = "^0.0.3", version = Version.create 0 0 3, expectedMatch = True }
    , { versionPattern = "^0.0.3", version = Version.create 0 0 4, expectedMatch = False }

    -- Comparators (AND semantics)
    , { versionPattern = ">=1.2.3 <2.0.0", version = Version.create 1 2 3, expectedMatch = True }
    , { versionPattern = ">=1.2.3 <2.0.0", version = Version.create 1 9 9, expectedMatch = True }
    , { versionPattern = ">=1.2.3 <2.0.0", version = Version.create 2 0 0, expectedMatch = False }
    , { versionPattern = ">=1.2.3 <2.0.0", version = Version.create 1 2 2, expectedMatch = False }
    , { versionPattern = ">1.2.3 <=1.2.5", version = Version.create 1 2 4, expectedMatch = True }
    , { versionPattern = ">1.2.3 <=1.2.5", version = Version.create 1 2 3, expectedMatch = False } -- strict >
    , { versionPattern = ">1.2.3 <=1.2.5", version = Version.create 1 2 6, expectedMatch = False }
    , { versionPattern = "<=1.0.0", version = Version.create 0 9 9, expectedMatch = True }
    , { versionPattern = "<=1.0.0", version = Version.create 1 0 0, expectedMatch = True }
    , { versionPattern = "<=1.0.0", version = Version.create 1 0 1, expectedMatch = False }
    , { versionPattern = ">=1.0.0", version = Version.create 1 0 0, expectedMatch = True }
    , { versionPattern = ">=1.0.0", version = Version.create 0 9 9, expectedMatch = False }

    -- HyphenRange (inclusive)
    , { versionPattern = "1.2.3 - 1.4.0", version = Version.create 1 2 3, expectedMatch = True } -- lower bound
    , { versionPattern = "1.2.3 - 1.4.0", version = Version.create 1 3 0, expectedMatch = True }
    , { versionPattern = "1.2.3 - 1.4.0", version = Version.create 1 4 0, expectedMatch = True } -- upper bound
    , { versionPattern = "1.2.3 - 1.4.0", version = Version.create 1 2 2, expectedMatch = False }
    , { versionPattern = "1.2.3 - 1.4.0", version = Version.create 1 4 1, expectedMatch = False }

    -- Whitespace robustness
    , { versionPattern = "  ~1.2.0 ", version = Version.create 1 2 9, expectedMatch = True }
    , { versionPattern = ">= 1.2.3   < 2.0.0", version = Version.create 1 5 0, expectedMatch = True }
    ]


matchesTest : Test
matchesTest =
    describe "Version Pattern Matches"
        [ parametrizedWithTitle testCases "matches" (\t -> "\"" ++ t.versionPattern ++ "\" ? \"" ++ Version.toString t.version ++ "\"") <|
            \{ versionPattern, version, expectedMatch } ->
                case VersionPattern.fromString versionPattern of
                    Ok pattern ->
                        let
                            actualMatch =
                                VersionPattern.matches pattern version
                        in
                        Expect.equal expectedMatch actualMatch

                    Err _ ->
                        Expect.fail ("Failed to parse version pattern: " ++ versionPattern)
        ]
