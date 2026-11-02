module Common.Components.KMComparison.Differ exposing
    ( DiffResult(..)
    , createDiff
    )

{-| What happened to one list of things when it became another.

Generic: there is no knowledge model in here. It diffs the **lists of ids** with
Myers (`jinjor/elm-diff`) and then asks the caller's `equalContent` whether a
thing that is on both sides is the same thing.

Three properties the callers depend on:

  - **Identity is the id, and order matters.** A child that moved is `Removed`
    and `Added`, not `NoChange` — the knowledge model's lists are ordered and a
    reorder is a change.
  - **The result is an order-preserving merge of both sides**, so a tree built
    from it reads as one tree rather than as two.
  - **It is flat.** One list per level of siblings; nesting is made by recursing
    and carrying a level, never by a nested result.

Generic enough for `packages/elm-vendor`, and it should move there the day a second
app wants it — not before.

-}

import Diff
import List.Extra as List


type DiffResult a
    = Added a
    | Removed a
    | Changed a a
    | NoChange a a


createDiff : (a -> id) -> (a -> a -> Bool) -> List a -> List a -> List (DiffResult a)
createDiff toId isEqual leftList rightList =
    let
        toDiffResult diff =
            case diff of
                Diff.Added id ->
                    List.find (\item -> toId item == id) rightList
                        |> Maybe.map Added

                Diff.Removed id ->
                    List.find (\item -> toId item == id) leftList
                        |> Maybe.map Removed

                Diff.NoChange id ->
                    let
                        leftItem =
                            List.find (\item -> toId item == id) leftList

                        rightItem =
                            List.find (\item -> toId item == id) rightList
                    in
                    case ( leftItem, rightItem ) of
                        ( Just left, Just right ) ->
                            if isEqual left right then
                                Just (NoChange left right)

                            else
                                Just (Changed left right)

                        _ ->
                            Nothing
    in
    List.filterMap toDiffResult (Diff.diff (List.map toId leftList) (List.map toId rightList))
