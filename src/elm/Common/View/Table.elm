module Common.View.Table exposing
    ( TableAction(..)
    , TableActionConfig
    , TableActionLabel(..)
    , TableConfig
    , TableFieldConfig
    , TableFieldValue(..)
    , view
    )

import Common.Html exposing (linkTo)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Msgs
import Routing


type TableFieldValue a
    = TextValue (a -> String)
    | BoolValue (a -> Bool)
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
    | TableActionButtonLink (a -> Routing.Route)
    | TableActionExternalLink (a -> String)
    | TableActionCustom ((msg -> Msgs.Msg) -> a -> Html Msgs.Msg)


type alias TableActionConfig a msg =
    { label : TableActionLabel
    , action : TableAction a msg
    , visible : a -> Bool
    }


type alias TableConfig a msg =
    { emptyMessage : String
    , fields : List (TableFieldConfig a)
    , actions : List (TableActionConfig a msg)
    , sortBy : a -> String
    }


view : TableConfig a msg -> (msg -> Msgs.Msg) -> List a -> Html Msgs.Msg
view config wrapMsg data =
    let
        tableData =
            List.sortBy config.sortBy data
    in
    table [ class "table index-table table-hover" ]
        [ tableHeader config
        , tableBody config wrapMsg tableData
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
    in
    case actionConfig.action of
        TableActionMsg createMsg ->
            a [ onClick <| createMsg wrapMsg item ] [ actionLabel ]

        TableActionLink getRoute ->
            linkTo (getRoute item) [] [ actionLabel ]

        TableActionButtonLink getRoute ->
            linkTo (getRoute item) [ class "btn btn-outline-primary" ] [ actionLabel ]

        TableActionExternalLink getUrl ->
            a [ href (getUrl item), target "_blank" ] [ actionLabel ]

        TableActionCustom getHtml ->
            getHtml wrapMsg item


bodyField : a -> TableFieldValue a -> Html Msgs.Msg
bodyField item fieldValue =
    let
        ( fieldClass, fieldContent ) =
            case fieldValue of
                TextValue getValue ->
                    ( "", text <| getValue item )

                BoolValue getValue ->
                    ( "td-bool", getBoolIcon <| getValue item )

                HtmlValue getValue ->
                    ( "", getValue item )
    in
    td [ class fieldClass ] [ fieldContent ]


getBoolIcon : Bool -> Html Msgs.Msg
getBoolIcon bool =
    if bool then
        i [ class "fa fa-check" ] []

    else
        i [ class "fa fa-times" ] []


tableEmpty : TableConfig a msg -> Html Msgs.Msg
tableEmpty config =
    let
        colspanValue =
            List.length config.fields + 1
    in
    tr []
        [ td [ colspan colspanValue, class "td-empty-table" ] [ text config.emptyMessage ] ]
