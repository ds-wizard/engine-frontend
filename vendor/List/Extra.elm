module List.Extra exposing (..)

import List exposing (..)
import Tuple


{-| Returns `Just` the element at the given index in the list,
or `Nothing` if the list is not long enough.
-}
getAt : Int -> List a -> Maybe a
getAt idx xs =
    List.head <| List.drop idx xs


removeAt : Int -> List a -> List a
removeAt idx xs =
    List.take idx xs ++ List.drop (idx + 1) xs


{-| Take a predicate and a list, return the index of the first element that satisfies the predicate. Otherwise, return `Nothing`. Indexing starts from 0.
findIndex isEven [1,2,3] == Just 1
findIndex isEven [1,3,5] == Nothing
findIndex isEven [1,2,4] == Just 1
-}
findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex p =
    head << findIndices p


{-| Take a predicate and a list, return indices of all elements satisfying the predicate. Otherwise, return empty list. Indexing starts from 0.
findIndices isEven [1,2,3] == [1]
findIndices isEven [1,3,5] == []
findIndices isEven [1,2,4] == [1,2]
-}
findIndices : (a -> Bool) -> List a -> List Int
findIndices p =
    map Tuple.first << filter (\( i, x ) -> p x) << indexedMap (,)


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.
find (\num -> num > 5) [2, 4, 6, 8] == Just 6
-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first
            else
                find predicate rest
