module Shared.Form.Field exposing
    ( dict
    , int
    , maybeString
    )

import Dict exposing (Dict)
import Form.Field as Field exposing (Field)


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
