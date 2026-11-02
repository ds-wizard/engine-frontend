module Dict.Extensions exposing (fromMaybeList)

import Dict exposing (Dict)


fromMaybeList : List ( comparable, Maybe a ) -> Dict comparable a
fromMaybeList list =
    let
        fold ( key, mbItem ) acc =
            case mbItem of
                Just item ->
                    Dict.insert key item acc

                Nothing ->
                    acc
    in
    List.foldl fold Dict.empty list
