module Common.Routes.FormerAddressTest exposing (suite)

import Common.Routes.FormerAddress as FormerAddress
import Expect
import Test exposing (Test, describe, test)
import Url


toCurrent : String -> Maybe String
toCurrent address =
    Url.fromString ("http://localhost" ++ address)
        |> Maybe.andThen FormerAddress.toCurrent
        |> Maybe.map (\url -> String.replace "http://localhost" "" (Url.toString url))


expectCurrent : String -> String -> Test
expectCurrent former current =
    test (former ++ " is " ++ current) <|
        \_ -> Expect.equal (Just current) (toCurrent former)


suite : Test
suite =
    let
        uuid =
            "8a703cfa-450f-421a-8819-875619ccb54d"
    in
    describe "Common.Routes.FormerAddress"
        [ describe "the prefix is dropped"
            [ expectCurrent ("/wizard/projects/" ++ uuid) ("/projects/" ++ uuid)
            , expectCurrent ("/wizard/projects/" ++ uuid ++ "/documents") ("/projects/" ++ uuid ++ "/documents")
            , expectCurrent "/wizard/projects?q=grant&page=2" "/projects?q=grant&page=2"
            , expectCurrent "/wizard/users" "/users"
            , expectCurrent "/wizard" "/"
            , expectCurrent "/wizard/" "/"
            ]
        , describe "the query and the fragment are kept"
            [ expectCurrent ("/wizard/projects/" ++ uuid ++ "?questionPath=a.b.c#comment") ("/projects/" ++ uuid ++ "?questionPath=a.b.c#comment")
            ]
        , describe "the editors are under what they make"
            [ expectCurrent "/wizard/knowledge-model-editors" "/knowledge-models/editors"
            , expectCurrent "/wizard/knowledge-model-editors/create" "/knowledge-models/editors/create"
            , expectCurrent ("/wizard/knowledge-model-editors/editor/" ++ uuid) ("/knowledge-models/editors/" ++ uuid)
            , expectCurrent ("/wizard/knowledge-model-editors/editor/" ++ uuid ++ "/phases") ("/knowledge-models/editors/" ++ uuid ++ "/phases")
            , expectCurrent ("/wizard/knowledge-model-editors/migration/" ++ uuid) ("/knowledge-models/editors/" ++ uuid ++ "/migration")
            , expectCurrent ("/wizard/knowledge-model-editors/publish/" ++ uuid) ("/knowledge-models/editors/" ++ uuid ++ "/publish")
            , expectCurrent "/wizard/document-template-editors" "/document-templates/editors"
            , expectCurrent ("/wizard/document-template-editors/" ++ uuid ++ "/preview") ("/document-templates/editors/" ++ uuid ++ "/preview")
            ]
        , describe "a current address is left alone"
            [ test "a page at the root" <|
                \_ -> Expect.equal Nothing (toCurrent ("/projects/" ++ uuid))
            , test "the root" <|
                \_ -> Expect.equal Nothing (toCurrent "/")
            , test "a path that only starts like the prefix" <|
                \_ -> Expect.equal Nothing (toCurrent "/wizardry")
            ]
        ]
