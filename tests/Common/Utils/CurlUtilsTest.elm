module Common.Utils.CurlUtilsTest exposing (parseCurlTest)

import Common.Utils.CurlUtils as CurlUtils
import Expect
import Test exposing (Test)


parseCurlTest : Test
parseCurlTest =
    let
        testCases =
            [ { name = "Simple GET request"
              , curlString = "curl https://api.example.com/data"
              , expected =
                    { method = "GET"
                    , url = "https://api.example.com/data"
                    , headers = []
                    , body = ""
                    }
              }
            , { name = "POST request with headers and body"
              , curlString = "curl -X POST https://api.example.com/data -H 'Content-Type: application/json' -d '{\"key\":\"value\"}'"
              , expected =
                    { method = "POST"
                    , url = "https://api.example.com/data"
                    , headers = [ ( "Content-Type", "application/json" ) ]
                    , body = "{\"key\":\"value\"}"
                    }
              }
            , { name = "POST request with multiple headers and double quotes"
              , curlString = "curl -X POST https://api.example.com/data -H \"Content-Type: application/json\" -H \"Authorization: Bearer token123\" -d '{\"key\":\"value\"}'"
              , expected =
                    { method = "POST"
                    , url = "https://api.example.com/data"
                    , headers = [ ( "Content-Type", "application/json" ), ( "Authorization", "Bearer token123" ) ]
                    , body = "{\"key\":\"value\"}"
                    }
              }
            , { name = "Multi-line CURL request"
              , curlString = """
                 curl -X PUT https://api.example.com/update \\
                 -H 'Content-Type: application/json' \\
                 -d '{"update":"data"}'
                 """
              , expected =
                    { method = "PUT"
                    , url = "https://api.example.com/update"
                    , headers = [ ( "Content-Type", "application/json" ) ]
                    , body = "{\"update\":\"data\"}"
                    }
              }
            , { name = "CURL with extra spaces and tabs"
              , curlString = "curl    -X   DELETE    https://api.example.com/item/123   -H   'Authorization: Bearer token123'   "
              , expected =
                    { method = "DELETE"
                    , url = "https://api.example.com/item/123"
                    , headers = [ ( "Authorization", "Bearer token123" ) ]
                    , body = ""
                    }
              }
            , { name = "CURL with extra unused params"
              , curlString = "curl -X GET https://api.example.com/data -e https://api.example.com/referer"
              , expected =
                    { method = "GET"
                    , url = "https://api.example.com/data"
                    , headers = []
                    , body = ""
                    }
              }
            ]
    in
    Test.describe "CurlUtils.parseCurl"
        (List.map
            (\testCase ->
                Test.test testCase.name <|
                    \_ ->
                        let
                            result =
                                CurlUtils.parseCurl testCase.curlString
                        in
                        Expect.equal result testCase.expected
            )
            testCases
        )
