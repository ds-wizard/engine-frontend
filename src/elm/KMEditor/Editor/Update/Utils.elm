module KMEditor.Editor.Update.Utils exposing (..)

import KMEditor.Common.Models.Events exposing (Event, Path, PathNode)
import Random.Pcg exposing (Seed)
import Utils exposing (getUuid)


updateInListWithSeed : List t -> Seed -> (t -> Bool) -> (Seed -> t -> ( Seed, t, Maybe Event )) -> ( Seed, List t, Maybe Event )
updateInListWithSeed list seed predicate updateFunction =
    let
        fn =
            \item ( currentSeed, items, currentEvent ) ->
                if predicate item then
                    let
                        ( updatedSeed, updatedItem, event ) =
                            updateFunction seed item
                    in
                    ( updatedSeed, items ++ [ updatedItem ], event )
                else
                    ( currentSeed, items ++ [ item ], currentEvent )
    in
    List.foldl fn ( seed, [], Nothing ) list


updateInList : List a -> (a -> Bool) -> (a -> a) -> List a
updateInList list predicate updateFunction =
    let
        fn =
            \item ->
                if predicate item then
                    updateFunction item
                else
                    item
    in
    List.map fn list


addChild : Seed -> List et -> (Bool -> Int -> ct -> et) -> (String -> ct) -> (ct -> Seed -> ( Event, Seed )) -> ( Seed, List et, Event )
addChild seed children createChildEditor newChild createAddChildEvent =
    let
        ( newUuid, seed2 ) =
            getUuid seed

        child =
            newChild newUuid

        newChildren =
            createChildEditor True (List.length children) child
                |> List.singleton
                |> List.append children

        ( event, newSeed ) =
            createAddChildEvent child seed2
    in
    ( newSeed, newChildren, event )


appendPath : Path -> PathNode -> Path
appendPath path pathNode =
    path ++ [ pathNode ]
