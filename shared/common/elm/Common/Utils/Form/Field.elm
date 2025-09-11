module Common.Utils.Form.Field exposing
    ( dict
    , int
    , maybeInt
    , maybeString
    )

import Dict exposing (Dict)
import Form.Field as Field exposing (Field)
import Maybe.Extra as Maybe


maybeString : Maybe String -> Field
maybeString =
    Field.string << Maybe.withDefault ""


dict : (a -> Field) -> Dict String a -> Field
dict valueField inputDict =
    let
        initEntry value =
            [ ( "key", Field.string <| Tuple.first value )
            , ( "value", valueField <| Tuple.second value )
            ]

        values =
            inputDict
                |> Dict.toList
                |> List.map (Field.group << initEntry)
    in
    Field.list values


int : Int -> Field
int =
    Field.string << String.fromInt


maybeInt : Maybe Int -> Field
maybeInt =
    Field.string << Maybe.unwrap "" String.fromInt
