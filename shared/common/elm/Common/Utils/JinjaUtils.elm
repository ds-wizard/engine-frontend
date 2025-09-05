module Common.Utils.JinjaUtils exposing
    ( JinjaParseResult
    , parseJinja
    )

import Common.Utils.RegexPatterns as RegexPatterns
import Regex exposing (Match)
import Set exposing (Set)
import String



-- PUBLIC API


type alias JinjaParseResult =
    { properties : List String
    , variablesNested : List String
    , secretsNested : List String
    }


{-| Parse all Jinja {{ ... }} expressions in `input`.

Returns:

  - properties: all normalized dotted paths (e.g. "user.name", "variables.first", "secrets.API\_KEY")
  - variablesNested: first segment after "variables" (e.g. "first" from "variables.first")
  - secretsNested: first segment after "secrets" (e.g. "API\_KEY" from "secrets.API\_KEY")

-}
parseJinja : String -> JinjaParseResult
parseJinja input =
    let
        -- Extract {{ ... }} contents
        expressions : List String
        expressions =
            case Regex.fromString "\\{\\{\\-?\\s*([\\s\\S]*?)\\s*\\-?\\}\\}" of
                Nothing ->
                    []

                Just re ->
                    Regex.find re input
                        |> List.filterMap firstCapture

        varsFromExpr : String -> List String
        varsFromExpr expr =
            let
                -- 1) Normalize bracket access to dotted form first
                normalized =
                    normalizeAccess expr

                -- 2) Strip any remaining string literals
                exprNoStrings =
                    stripStrings normalized

                -- 3) Capture identifiers/dotted paths NOT followed by '(' (skip function calls)
                re =
                    RegexPatterns.fromString "\\b([A-Za-z_][A-Za-z0-9_]*(?:\\.[A-Za-z_][A-Za-z0-9_]*)*)\\b(?!\\s*\\()"
            in
            Regex.find re exprNoStrings
                |> List.filterMap firstCapture
                |> List.filter (not << isStopWord)
                |> List.filter (not << isNumericLike)

        allProps : List String
        allProps =
            expressions
                |> List.concatMap varsFromExpr
                |> dedupPreserveOrder

        variablesNested : List String
        variablesNested =
            allProps
                |> List.filter (String.startsWith "variables.")
                |> List.filterMap (firstSegmentAfter "variables")
                |> dedupPreserveOrder

        secretsNested : List String
        secretsNested =
            allProps
                |> List.filter (String.startsWith "secrets.")
                |> List.filterMap (firstSegmentAfter "secrets")
                |> dedupPreserveOrder
    in
    { properties = allProps
    , variablesNested = variablesNested
    , secretsNested = secretsNested
    }



-- HELPERS


firstCapture : Match -> Maybe String
firstCapture m =
    case m.submatches of
        (Just s) :: _ ->
            let
                t =
                    String.trim s
            in
            if t == "" then
                Nothing

            else
                Just t

        _ ->
            Nothing



-- Replace ["key"] or ['key'] with .key (supports multiple segments)


normalizeAccess : String -> String
normalizeAccess s =
    let
        accessRe =
            RegexPatterns.fromString "\\[\\s*(?:\"([A-Za-z0-9_]+)\"|'([A-Za-z0-9_]+)')\\s*\\]"

        firstJust : List (Maybe String) -> Maybe String
        firstJust subs =
            case subs of
                [] ->
                    Nothing

                x :: xs ->
                    case x of
                        Just v ->
                            Just v

                        Nothing ->
                            firstJust xs
    in
    Regex.replace accessRe
        (\m ->
            "." ++ Maybe.withDefault "" (firstJust m.submatches)
        )
        s


stripStrings : String -> String
stripStrings s =
    case Regex.fromString "\"(?:\\\\.|[^\"\\\\])*\"|'(?:\\\\.|[^'\\\\])*'" of
        Nothing ->
            s

        Just re ->
            Regex.replace re (\_ -> "") s


isStopWord : String -> Bool
isStopWord word =
    let
        stops =
            Set.fromList
                [ "true"
                , "false"
                , "none"
                , "null"
                , "and"
                , "or"
                , "not"
                , "in"
                , "is"
                , "if"
                , "else"
                , "elif"
                , "for"
                , "endfor"
                , "endif"
                , "set"
                , "loop"
                , "self"
                ]
    in
    Set.member (String.toLower word) stops


isNumericLike : String -> Bool
isNumericLike s =
    case Regex.fromString "^[-+]?\\d+(?:\\.\\d+)?$" of
        Nothing ->
            False

        Just re ->
            not (List.isEmpty (Regex.find re s))


dedupPreserveOrder : List String -> List String
dedupPreserveOrder items =
    let
        step x ( seen, acc ) =
            if Set.member x seen then
                ( seen, acc )

            else
                ( Set.insert x seen, acc ++ [ x ] )
    in
    items
        |> List.foldl step ( Set.empty, [] )
        |> Tuple.second



-- Given "root.child.more", return Just "child" when root == "root".


firstSegmentAfter : String -> String -> Maybe String
firstSegmentAfter root path =
    case String.split "." path of
        r :: p :: _ ->
            if r == root then
                Just p

            else
                Nothing

        _ ->
            Nothing
