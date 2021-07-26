module ValueList exposing
    ( Msg
    , ValueList
    , init
    , update
    , view
    )

import Html exposing (Html, a, button, code, div, i, input, li, text, ul)
import Html.Attributes exposing (attribute, class, type_, value)
import Html.Events exposing (onClick, onInput)


type alias ValueList =
    { list : List String
    , current : String
    , dirty : Bool
    }


type Msg
    = Input String
    | Add
    | RemoveAt Int


init : List String -> ValueList
init values =
    { list = values
    , current = ""
    , dirty = False
    }


update : Msg -> ValueList -> ValueList
update msg valueList =
    case msg of
        Input value ->
            { valueList | current = value }

        Add ->
            addCurrent valueList

        RemoveAt i ->
            removeAt i valueList


addCurrent : ValueList -> ValueList
addCurrent valueList =
    if String.isEmpty valueList.current then
        valueList

    else
        { list = valueList.list ++ [ valueList.current ]
        , current = ""
        , dirty = True
        }


removeAt : Int -> ValueList -> ValueList
removeAt i valueList =
    { valueList
        | list = List.take i valueList.list ++ List.drop (i + 1) valueList.list
        , dirty = True
    }


view : ValueList -> Html Msg
view valueList =
    div []
        [ div [ class "input-group" ]
            [ input
                [ class "form-control"
                , type_ "text"
                , onInput Input
                , value valueList.current
                , attribute "data-cy" "value-list_input"
                ]
                []
            , div [ class "input-group-append" ]
                [ button
                    [ class "btn btn-secondary"
                    , onClick Add
                    , attribute "data-cy" "value-list_add-button"
                    ]
                    [ text "Add" ]
                ]
            ]
        , ul [ class "mt-2 list-group list-group-hover" ] (List.indexedMap viewItem valueList.list)
        ]


viewItem : Int -> String -> Html Msg
viewItem index title =
    li
        [ class "list-group-item d-flex justify-content-between align-items-center"
        , attribute "data-cy" "value-list_item"
        ]
        [ text title
        , a
            [ class "text-danger"
            , onClick <| RemoveAt index
            , attribute "data-cy" "value-list_remove-button"
            ]
            [ i [ class "fa fa-times mr-1" ] []
            , text "Remove"
            ]
        ]
