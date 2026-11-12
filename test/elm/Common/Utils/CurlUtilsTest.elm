module Common.Utils.CurlUtilsTest exposing (suite)

{-| Reading a cURL command into an API integration's request, as the KM
editor's cURL import does.
-}

import Common.Utils.CurlUtils as CurlUtils
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Common.Utils.CurlUtils.parseCurl"
        [ describe "body"
            [ test "--data is read as -d is" <|
                \_ ->
                    Expect.equal
                        { method = "POST", url = "https://example.invalid/ui-test", headers = [], body = "{\"foo\":\"changed\"}" }
                        (CurlUtils.parseCurl "curl 'https://example.invalid/ui-test' -X POST --data '{\"foo\":\"changed\"}'")
            , test "-d, --data-raw, --data-binary and --data-ascii all give the body" <|
                \_ ->
                    Expect.equal [ "a=1", "a=1", "a=1", "a=1" ]
                        (List.map (.body << CurlUtils.parseCurl)
                            [ "curl https://x.invalid -d a=1"
                            , "curl https://x.invalid --data-raw 'a=1'"
                            , "curl https://x.invalid --data-binary \"a=1\""
                            , "curl https://x.invalid --data-ascii a=1"
                            ]
                        )
            , test "several data options are joined with &" <|
                \_ ->
                    Expect.equal "a=1&b=2"
                        (CurlUtils.parseCurl "curl https://x.invalid -d a=1 --data b=2").body
            , test "data makes the request a POST unless a method is given" <|
                \_ ->
                    Expect.equal [ "POST", "PUT", "PUT", "GET" ]
                        (List.map (.method << CurlUtils.parseCurl)
                            [ "curl https://x.invalid --data a=1"
                            , "curl https://x.invalid --data a=1 -X PUT"
                            , "curl -X PUT https://x.invalid --data a=1"
                            , "curl https://x.invalid"
                            ]
                        )
            , test "-G puts the data in the URL's query and sends no body" <|
                \_ ->
                    Expect.equal
                        { method = "GET", url = "https://x.invalid/search?q=1&page=2", headers = [], body = "" }
                        (CurlUtils.parseCurl "curl -G https://x.invalid/search -d q=1 -d page=2")
            , test "--json sends its data and says it is JSON" <|
                \_ ->
                    Expect.equal
                        { method = "POST"
                        , url = "https://x.invalid"
                        , headers = [ ( "Content-Type", "application/json" ), ( "Accept", "application/json" ) ]
                        , body = "{\"a\":1}"
                        }
                        (CurlUtils.parseCurl "curl https://x.invalid --json '{\"a\":1}'")
            , test "--json keeps a content type that is given" <|
                \_ ->
                    Expect.equal [ ( "content-type", "application/vnd.api+json" ), ( "Accept", "application/json" ) ]
                        (CurlUtils.parseCurl "curl https://x.invalid -H 'content-type: application/vnd.api+json' --json '{}'").headers
            ]
        , describe "method"
            [ test "-X, a joined -X and --request" <|
                \_ ->
                    Expect.equal [ "DELETE", "PATCH", "PUT" ]
                        (List.map (.method << CurlUtils.parseCurl)
                            [ "curl -X DELETE https://x.invalid"
                            , "curl -XPATCH https://x.invalid"
                            , "curl --request PUT https://x.invalid"
                            ]
                        )
            , test "-I is a HEAD" <|
                \_ ->
                    Expect.equal "HEAD" (CurlUtils.parseCurl "curl -I https://x.invalid").method
            ]
        , describe "headers"
            [ test "-H and --header, in order" <|
                \_ ->
                    Expect.equal [ ( "Accept", "application/json" ), ( "Authorization", "Bearer a:b" ) ]
                        (CurlUtils.parseCurl "curl https://x.invalid -H 'Accept: application/json' --header \"Authorization: Bearer a:b\"").headers
            , test "a header without a name is left out" <|
                \_ ->
                    Expect.equal [] (CurlUtils.parseCurl "curl https://x.invalid -H ': nothing' -H 'X-Empty;'").headers
            ]
        , describe "URL"
            [ test "flags without a value do not take the URL" <|
                \_ ->
                    Expect.equal "https://x.invalid/a"
                        (CurlUtils.parseCurl "curl -s -L -k --compressed --insecure https://x.invalid/a").url
            , test "joined flags do not take the URL" <|
                \_ ->
                    Expect.equal "https://x.invalid/a" (CurlUtils.parseCurl "curl -sSL https://x.invalid/a").url
            , test "options with a value are skipped with it" <|
                \_ ->
                    Expect.equal "https://x.invalid/a"
                        (CurlUtils.parseCurl "curl -u user:secret -A agent -b 'a=1' --max-time 5 https://x.invalid/a").url
            , test "--url gives it" <|
                \_ ->
                    Expect.equal "https://x.invalid/a" (CurlUtils.parseCurl "curl --url https://x.invalid/a -X POST").url
            , test "the first URL is the one" <|
                \_ ->
                    Expect.equal "https://x.invalid/a" (CurlUtils.parseCurl "curl https://x.invalid/a https://x.invalid/b").url
            ]
        , describe "quoting"
            [ test "a command copied from a browser, over several lines" <|
                \_ ->
                    Expect.equal
                        { method = "POST"
                        , url = "https://api.x.invalid/items?a=1&b=2"
                        , headers = [ ( "accept", "*/*" ), ( "content-type", "application/json" ) ]
                        , body = "{\"name\":\"it's\"}"
                        }
                        (CurlUtils.parseCurl
                            ("curl 'https://api.x.invalid/items?a=1&b=2' \\\n"
                                ++ "  -H 'accept: */*' \\\n"
                                ++ "  -H 'content-type: application/json' \\\n"
                                ++ "  --data-raw $'{\"name\":\"it\\'s\"}' \\\n"
                                ++ "  --compressed"
                            )
                        )
            , test "escapes in double quotes" <|
                \_ ->
                    Expect.equal "{\"a\":\"$x\\n\"}"
                        (CurlUtils.parseCurl "curl https://x.invalid -d \"{\\\"a\\\":\\\"\\$x\\n\\\"}\"").body
            , test "escapes in $'…'" <|
                \_ ->
                    Expect.equal "line\nnext é ’"
                        (CurlUtils.parseCurl "curl https://x.invalid -d $'line\\nnext \\xe9 \\u2019'").body
            , test "an empty quoted value is a value" <|
                \_ ->
                    Expect.equal { method = "POST", url = "https://x.invalid", headers = [], body = "" }
                        (CurlUtils.parseCurl "curl -d '' https://x.invalid")
            ]
        , test "an empty command is a GET with nothing" <|
            \_ ->
                Expect.equal { method = "GET", url = "", headers = [], body = "" } (CurlUtils.parseCurl "")
        ]
