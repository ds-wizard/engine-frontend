module Dsw.RequestParamsTest exposing (suite)

{-| What the client adds to its requests: never a workspace, and the
tenant exactly where the server has to be told.
-}

import Dsw.Data.RequestParams exposing (requestParams)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Dsw.Data.RequestParams"
        [ test "lists take nothing" <|
            \_ -> requestParams { method = "GET", path = "/projects" } |> Expect.equal []
        , test "roles are the tenant's" <|
            \_ -> requestParams { method = "GET", path = "/roles" } |> Expect.equal [ ( "tenant", "true" ) ]
        , test "an imported knowledge model is the tenant's" <|
            \_ -> requestParams { method = "POST", path = "/knowledge-model-packages/bundle" } |> Expect.equal [ ( "tenant", "true" ) ]
        , test "a pulled template is the tenant's" <|
            \_ -> requestParams { method = "POST", path = "/document-templates/a:b:1.0.0/pull" } |> Expect.equal [ ( "tenant", "true" ) ]
        , test "a project is created without a scope" <|
            \_ -> requestParams { method = "POST", path = "/projects" } |> Expect.equal []
        ]
