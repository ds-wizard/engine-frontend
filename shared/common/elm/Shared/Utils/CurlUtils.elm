module Shared.Utils.CurlUtils exposing
    ( Request
    , parseCurl
    )

import Maybe.Extra as Maybe
import Regex exposing (Regex)
import Shared.Utils.RegexPatterns as RegexPatterns
import String.Extra as String



-- Define a structure for parsed requests


type alias Request =
    { method : String
    , url : String
    , headers : List ( String, String )
    , body : String
    }



-- Main parser


parseCurl : String -> Request
parseCurl curlString =
    let
        defaultRequest =
            { method = "GET"
            , url = ""
            , headers = []
            , body = ""
            }

        tokenRegex =
            RegexPatterns.fromString "([\"'])(.*?)(\\1)|([^\"'\\s]+)(?=\\s*|\\s*$)"

        tokens =
            Regex.find tokenRegex curlString
                |> List.map .match
                |> List.filter ((/=) "\\")
    in
    parseTokens tokens defaultRequest


parseTokens : List String -> Request -> Request
parseTokens tokens request =
    case tokens of
        [] ->
            request

        "curl" :: rest ->
            parseTokens rest request

        "-X" :: method :: rest ->
            parseTokens rest { request | method = method }

        "-H" :: header :: rest ->
            case parseHeader header of
                Just ( key, value ) ->
                    parseTokens rest { request | headers = request.headers ++ [ ( key, value ) ] }

                Nothing ->
                    parseTokens rest request

        "-d" :: body :: rest ->
            parseTokens rest { request | body = String.stripQuotes body }

        url :: rest ->
            if String.startsWith "-" url then
                parseTokens (List.drop 1 rest) request

            else
                parseTokens rest { request | url = String.stripQuotes url }


parseHeader : String -> Maybe ( String, String )
parseHeader header =
    let
        headerRe =
            RegexPatterns.fromString "^(.*?):\\s*(.*)$"

        matches =
            Regex.find headerRe (String.stripQuotes header)
    in
    case matches of
        match :: _ ->
            case match.submatches of
                (Just key) :: (Just value) :: _ ->
                    Just ( key, value )

                _ ->
                    Nothing

        [] ->
            Nothing
