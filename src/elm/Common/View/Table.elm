module Common.View.Table exposing (..)

import Common.Html exposing (emptyNode, linkTo)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Msgs
import Routing


type TableFieldValue a
    = TextValue (a -> String)
    | HtmlValue (a -> Html Msgs.Msg)


type alias TableFieldConfig a =
    { label : String
    , getValue : TableFieldValue a
    }


type TableActionLabel
    = TableActionText String
    | TableActionIcon String


type TableAction a msg
    = TableActionMsg ((msg -> Msgs.Msg) -> a -> Msgs.Msg)
    | TableActionLink (a -> Routing.Route)


type alias TableActionConfig a msg =
    { label : TableActionLabel
    , action : TableAction a msg
    , visible : a -> Bool
    }


type alias TableConfig a msg =
    { emptyMessage : String
    , fields : List (TableFieldConfig a)
    , actions : List (TableActionConfig a msg)
    }


indexTable : TableConfig a msg -> (msg -> Msgs.Msg) -> List a -> Html Msgs.Msg
indexTable config wrapMsg data =
    table [ class "table index-table" ]
        [ tableHeader config
        , tableBody config wrapMsg data
        ]


tableHeader : TableConfig a msg -> Html Msgs.Msg
tableHeader config =
    let
        labelFields =
            List.map (headerField << .label) config.fields

        actionsField =
            [ th [] [ text "Actions" ] ]
    in
    thead []
        [ tr [] (labelFields ++ actionsField) ]


headerField : String -> Html Msgs.Msg
headerField name =
    th [] [ text name ]


tableBody : TableConfig a msg -> (msg -> Msgs.Msg) -> List a -> Html Msgs.Msg
tableBody config wrapMsg data =
    if List.isEmpty data then
        tableEmpty config
    else
        tbody [] (List.map (tableRow config wrapMsg) data)


tableRow : TableConfig a msg -> (msg -> Msgs.Msg) -> a -> Html Msgs.Msg
tableRow config wrapMsg item =
    let
        valueFields =
            List.map (bodyField item << .getValue) config.fields

        actions =
            [ tableRowActions config wrapMsg item ]
    in
    tr [] (valueFields ++ actions)


tableRowActions : TableConfig a msg -> (msg -> Msgs.Msg) -> a -> Html Msgs.Msg
tableRowActions config wrapMsg item =
    let
        actions =
            config.actions
                |> List.filter (\a -> a.visible item)
                |> List.map (tableAction wrapMsg item)
    in
    td [ class "table-actions" ] actions


tableAction : (msg -> Msgs.Msg) -> a -> TableActionConfig a msg -> Html Msgs.Msg
tableAction wrapMsg item actionConfig =
    let
        actionLabel =
            case actionConfig.label of
                TableActionText str ->
                    text str

                TableActionIcon iconClass ->
                    i [ class iconClass ] []

        actionElement =
            case actionConfig.action of
                TableActionMsg createMsg ->
                    a [ onClick <| createMsg wrapMsg item ]

                TableActionLink getRoute ->
                    linkTo (getRoute item) []
    in
    actionElement [ actionLabel ]


bodyField : a -> TableFieldValue a -> Html Msgs.Msg
bodyField item fieldValue =
    let
        fieldContent =
            case fieldValue of
                TextValue getValue ->
                    text <| getValue item

                HtmlValue getValue ->
                    getValue item
    in
    td [] [ fieldContent ]


tableEmpty : TableConfig a msg -> Html Msgs.Msg
tableEmpty config =
    let
        colspanValue =
            List.length config.fields + 1
    in
    tr []
        [ td [ colspan colspanValue, class "td-empty-table" ] [ text config.emptyMessage ] ]
