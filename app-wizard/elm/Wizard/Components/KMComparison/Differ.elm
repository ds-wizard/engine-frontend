module Wizard.Components.KMComparison.Differ exposing
    ( DiffResult(..)
    , createDiff
    )

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
        leftIds =
            List.map toId leftList

        rightIds =
            List.map toId rightList

        diffs =
            Diff.diff leftIds rightIds
    in
    List.filterMap
        (\diff ->
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
                            List.filter (\item -> toId item == id) leftList |> List.head

                        rightItem =
                            List.filter (\item -> toId item == id) rightList |> List.head
                    in
                    case ( leftItem, rightItem ) of
                        ( Just l, Just r ) ->
                            if isEqual l r then
                                Just (NoChange l r)

                            else
                                Just (Changed l r)

                        _ ->
                            Nothing
        )
        diffs
