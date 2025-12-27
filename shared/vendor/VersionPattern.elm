module VersionPattern exposing
    ( VersionPattern(..)
    , fromString
    , matches
    )

import Parser as P exposing ((|.), (|=), Parser)
import Version exposing (Version)


type VersionPattern
    = Exact Version
    | Caret Version
    | Tilde Version
    | Comparators (List Comparator)
    | HyphenRange Version Version
    | Wildcard Wildcard


type Wildcard
    = Any
    | AnyMinor Int
    | AnyPatch Int Int


type alias Comparator =
    { op : Operator
    , version : Version
    }


type Operator
    = GreaterThan
    | GreaterThanOrEqual
    | LessThan
    | LessThanOrEqual


fromString : String -> Result (List P.DeadEnd) VersionPattern
fromString str =
    str
        |> String.trim
        |> P.run
            (P.succeed identity
                |. P.spaces
                |= versionPatternParser
                |. P.spaces
                |. P.end
            )


matches : VersionPattern -> Version -> Bool
matches pattern version =
    case pattern of
        Exact v ->
            Version.compare version v == EQ

        Caret v ->
            let
                ( lower, upper ) =
                    caretBounds v
            in
            (Version.compare version lower /= LT)
                && (Version.compare version upper == LT)

        Tilde v ->
            let
                ( lower, upper ) =
                    tildeBounds v
            in
            (Version.compare version lower /= LT)
                && (Version.compare version upper == LT)

        Comparators comps ->
            List.all (\c -> matchesComparator c version) comps

        HyphenRange a b ->
            (Version.compare version a /= LT)
                && (Version.compare version b /= GT)

        Wildcard wc ->
            matchesWildcard wc version



-- PARSERS --------------------------------------------------------------------


versionPatternParser : Parser VersionPattern
versionPatternParser =
    P.oneOf
        [ P.backtrackable hyphenRangeParser
        , P.backtrackable exactParser
        , P.backtrackable wildcardParser
        , caretParser
        , tildeParser
        , comparatorsParser
        , wildcardAnyParser
        ]


exactParser : Parser VersionPattern
exactParser =
    versionParser |> P.map Exact


caretParser : Parser VersionPattern
caretParser =
    P.succeed Caret
        |. P.symbol "^"
        |. P.spaces
        |= versionParser


tildeParser : Parser VersionPattern
tildeParser =
    P.succeed Tilde
        |. P.symbol "~"
        |. P.spaces
        |= versionParser


hyphenRangeParser : Parser VersionPattern
hyphenRangeParser =
    P.succeed HyphenRange
        |= versionParser
        |. P.spaces
        |. P.symbol "-"
        |. P.spaces
        |= versionParser



-- WILDCARDS


wildcardAnyParser : Parser VersionPattern
wildcardAnyParser =
    P.oneOf
        [ P.symbol "*" |> P.map (\_ -> Wildcard Any)
        , P.keyword "x" |> P.map (\_ -> Wildcard Any)
        , P.keyword "X" |> P.map (\_ -> Wildcard Any)
        ]


wildcardParser : P.Parser VersionPattern
wildcardParser =
    P.succeed (\maj rest -> rest maj)
        |= digitsInt
        |. P.symbol "."
        |= P.oneOf
            [ -- 1.x / 1.*
              P.succeed (\maj -> Wildcard (AnyMinor maj))
                |. wildcardToken

            -- 1.2.x / 1.2.*
            , P.succeed (\min maj -> Wildcard (AnyPatch maj min))
                |= digitsInt
                |. P.symbol "."
                |. wildcardToken
            ]


wildcardToken : Parser ()
wildcardToken =
    P.oneOf
        [ P.symbol "*" |> P.map (\_ -> ())
        , P.keyword "x" |> P.map (\_ -> ())
        , P.keyword "X" |> P.map (\_ -> ())
        ]



-- COMPARATORS
-- Supports 1+ comparators separated by whitespace, e.g.:
--   ">=1.2.3 <2.0.0"
--   "<=2"
--   ">1.0.0"
--
-- NOTE: npm also allows combining ranges with "||" etc.
-- This parser intentionally does NOT parse OR-ranges.


comparatorsParser : Parser VersionPattern
comparatorsParser =
    P.succeed (\first rest -> Comparators (first :: rest))
        |= comparatorParser
        |= comparatorTailParser


comparatorTailParser : Parser (List Comparator)
comparatorTailParser =
    P.loop [] comparatorTailStep


comparatorTailStep : List Comparator -> Parser (P.Step (List Comparator) (List Comparator))
comparatorTailStep revAcc =
    P.oneOf
        [ P.succeed (\c -> P.Loop (c :: revAcc))
            |= spacedComparator
        , P.succeed (P.Done (List.reverse revAcc))
        ]


spacedComparator : Parser Comparator
spacedComparator =
    P.succeed identity
        |. spaces1
        |= comparatorParser


comparatorParser : Parser Comparator
comparatorParser =
    P.succeed (\op v -> { op = op, version = v })
        |= operatorParser
        |. P.spaces
        |= versionParser


operatorParser : Parser Operator
operatorParser =
    P.oneOf
        [ P.symbol ">=" |> P.map (\_ -> GreaterThanOrEqual)
        , P.symbol "<=" |> P.map (\_ -> LessThanOrEqual)
        , P.symbol ">" |> P.map (\_ -> GreaterThan)
        , P.symbol "<" |> P.map (\_ -> LessThan)
        ]



-- VERSION
-- Accepts "1", "1.2", "1.2.3" and defaults missing to 0


versionParser : Parser Version
versionParser =
    P.succeed Version.create
        |= digitsInt
        |= optionalPart dotInt 0
        |= optionalPart dotInt 0


optionalPart : Parser a -> a -> Parser a
optionalPart p defaultValue =
    P.oneOf
        [ p
        , P.succeed defaultValue
        ]


dotInt : Parser Int
dotInt =
    P.succeed identity
        |. P.symbol "."
        |= digitsInt



-- HELPERS


digitsInt : Parser Int
digitsInt =
    P.getChompedString
        (P.succeed ()
            |. P.chompIf Char.isDigit
            |. P.chompWhile Char.isDigit
        )
        |> P.andThen
            (\s ->
                case String.toInt s of
                    Just n ->
                        P.succeed n

                    Nothing ->
                        P.problem "Invalid int"
            )


spaces1 : Parser ()
spaces1 =
    P.chompIf isSpace
        |> P.andThen (\_ -> P.chompWhile isSpace)


isSpace : Char -> Bool
isSpace c =
    c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}'



-- COMPARATORS ----------------------------------------------------------------


matchesComparator : Comparator -> Version -> Bool
matchesComparator c v =
    case c.op of
        GreaterThan ->
            Version.compare v c.version == GT

        GreaterThanOrEqual ->
            let
                ord =
                    Version.compare v c.version
            in
            ord == GT || ord == EQ

        LessThan ->
            Version.compare v c.version == LT

        LessThanOrEqual ->
            let
                ord =
                    Version.compare v c.version
            in
            ord == LT || ord == EQ


matchesWildcard : Wildcard -> Version -> Bool
matchesWildcard wc v =
    case wc of
        Any ->
            True

        AnyMinor maj ->
            Version.getMajor v == maj

        AnyPatch maj min ->
            Version.getMajor v == maj && Version.getMinor v == min


caretBounds : Version -> ( Version, Version )
caretBounds v =
    -- npm-style caret rules:
    -- ^1.2.3  => >=1.2.3 <2.0.0
    -- ^0.2.3  => >=0.2.3 <0.3.0
    -- ^0.0.3  => >=0.0.3 <0.0.4
    let
        maj =
            Version.getMajor v

        min =
            Version.getMinor v

        pat =
            Version.getPatch v
    in
    if maj /= 0 then
        ( v, Version.create (maj + 1) 0 0 )

    else if min /= 0 then
        ( v, Version.create 0 (min + 1) 0 )

    else
        ( v, Version.create 0 0 (pat + 1) )


tildeBounds : Version -> ( Version, Version )
tildeBounds v =
    -- ~1.2.3 => >=1.2.3 <1.3.0
    let
        maj =
            Version.getMajor v

        min =
            Version.getMinor v
    in
    ( v, Version.create maj (min + 1) 0 )
