module Common.Components.KMComparison.DifferTest exposing (suite)

import Common.Components.KMComparison.Differ as Differ exposing (DiffResult(..))
import Expect
import Test exposing (Test, describe, test)


type alias Item =
    { id : String, label : String }


item : String -> String -> Item
item id label =
    { id = id, label = label }


diff : List Item -> List Item -> List (DiffResult Item)
diff =
    Differ.createDiff .id (\left right -> left.label == right.label)


suite : Test
suite =
    describe "Common.Components.KMComparison.Differ"
        [ test "the same list is unchanged all the way down" <|
            \_ ->
                diff [ item "a" "A", item "b" "B" ] [ item "a" "A", item "b" "B" ]
                    |> Expect.equal
                        [ NoChange (item "a" "A") (item "a" "A")
                        , NoChange (item "b" "B") (item "b" "B")
                        ]
        , test "an item on the right only is added" <|
            \_ ->
                diff [ item "a" "A" ] [ item "a" "A", item "b" "B" ]
                    |> Expect.equal
                        [ NoChange (item "a" "A") (item "a" "A")
                        , Added (item "b" "B")
                        ]
        , test "an item on the left only is removed" <|
            \_ ->
                diff [ item "a" "A", item "b" "B" ] [ item "a" "A" ]
                    |> Expect.equal
                        [ NoChange (item "a" "A") (item "a" "A")
                        , Removed (item "b" "B")
                        ]
        , test "an item whose content differs is changed, and carries both sides" <|
            \_ ->
                diff [ item "a" "A" ] [ item "a" "A!" ]
                    |> Expect.equal [ Changed (item "a" "A") (item "a" "A!") ]
        , test "a reordered item is removed and added, not unchanged" <|
            \_ ->
                diff [ item "a" "A", item "b" "B" ] [ item "b" "B", item "a" "A" ]
                    |> Expect.equal
                        [ Removed (item "a" "A")
                        , NoChange (item "b" "B") (item "b" "B")
                        , Added (item "a" "A")
                        ]
        , test "one added and a different one removed produce two rows, in the order the lists imply" <|
            \_ ->
                diff [ item "a" "A", item "b" "B" ] [ item "a" "A", item "c" "C" ]
                    |> Expect.equal
                        [ NoChange (item "a" "A") (item "a" "A")
                        , Removed (item "b" "B")
                        , Added (item "c" "C")
                        ]
        , test "two empty lists have nothing to say" <|
            \_ ->
                diff [] [] |> Expect.equal []
        , test "everything is added when the left side is empty" <|
            \_ ->
                diff [] [ item "a" "A", item "b" "B" ]
                    |> Expect.equal [ Added (item "a" "A"), Added (item "b" "B") ]
        ]
