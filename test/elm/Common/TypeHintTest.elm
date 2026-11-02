module Common.TypeHintTest exposing
    ( debounceTests
    , responseTests
    )

import Common.Components.TypeHint.Internal as Internal
import Expect
import Test exposing (Test, describe, test)


debounceTests : Test
debounceTests =
    describe "debounce"
        [ test "only the latest keystroke's timer fetches" <|
            \_ ->
                let
                    ( afterFirst, firstTag ) =
                        Internal.typed Internal.init

                    ( afterSecond, secondTag ) =
                        Internal.typed afterFirst
                in
                ( Internal.isLatest firstTag afterSecond, Internal.isLatest secondTag afterSecond )
                    |> Expect.equal ( False, True )
        , test "opening supersedes a timer still running" <|
            \_ ->
                let
                    ( typed, tag ) =
                        Internal.typed Internal.init
                in
                Internal.isLatest tag (Internal.requested typed)
                    |> Expect.equal False
        , test "typing starts loading at once" <|
            \_ ->
                Internal.typed Internal.init
                    |> Tuple.first
                    |> .loading
                    |> Expect.equal True
        , test "typing clears a previous error" <|
            \_ ->
                Internal.init
                    |> Internal.failed "Unable to get knowledge models."
                    |> Internal.typed
                    |> Tuple.first
                    |> .error
                    |> Expect.equal Nothing
        ]


responseTests : Test
responseTests =
    describe "responses"
        [ test "a response for the current query is accepted" <|
            \_ ->
                Internal.accepts { response = "data", current = "data" }
                    |> Expect.equal True
        , test "a late response for an older query is dropped" <|
            \_ ->
                Internal.accepts { response = "da", current = "data" }
                    |> Expect.equal False
        , test "received results replace the old ones and stop loading" <|
            \_ ->
                Internal.init
                    |> Internal.received [ "old" ]
                    |> Internal.typed
                    |> Tuple.first
                    |> Internal.received [ "new" ]
                    |> (\search -> ( search.results, search.loading ))
                    |> Expect.equal ( [ "new" ], False )
        , test "the old results stay visible while the next query loads" <|
            \_ ->
                Internal.init
                    |> Internal.received [ "old" ]
                    |> Internal.typed
                    |> Tuple.first
                    |> .results
                    |> Expect.equal [ "old" ]
        , test "a failure empties the results and says why" <|
            \_ ->
                Internal.init
                    |> Internal.received [ "old" ]
                    |> Internal.failed "Unable to get knowledge models."
                    |> (\search -> ( search.results, search.error, search.loading ))
                    |> Expect.equal ( [], Just "Unable to get knowledge models.", False )
        ]
