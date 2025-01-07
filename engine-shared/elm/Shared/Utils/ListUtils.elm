module Shared.Utils.ListUtils exposing (findNextInfinite, findPreviousInfinite)

import List.Extra as List


findPrevious : a -> List a -> Maybe a
findPrevious item list =
    case list of
        x :: y :: rest ->
            if item == y then
                Just x

            else
                findPrevious item (y :: rest)

        _ ->
            Nothing


findPreviousInfinite : a -> List a -> Maybe a
findPreviousInfinite item list =
    if List.head list == Just item then
        List.last list

    else
        findPrevious item list


findNext : a -> List a -> Maybe a
findNext item list =
    case list of
        x :: y :: rest ->
            if item == x then
                Just y

            else
                findNext item (y :: rest)

        _ ->
            Nothing


findNextInfinite : a -> List a -> Maybe a
findNextInfinite item list =
    if List.last list == Just item then
        List.head list

    else
        findNext item list
