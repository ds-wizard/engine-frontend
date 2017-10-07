module KnowledgeModels.Index.View exposing (..)

import Common.Html exposing (pageHeader)
import Html exposing (Html, a, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href)
import Msgs exposing (Msg)


view : Html Msg
view =
    div []
        [ pageHeader "Knowledge model" indexActions
        , kmTable
        ]


indexActions : List (Html Msg)
indexActions =
    [ a [ class "btn btn-primary" ] [ text "Create KM" ]
    ]


kmTable : Html Msg
kmTable =
    table [ class "table" ]
        [ kmTableHeader
        , kmTableBody
        ]


kmTableHeader : Html Msg
kmTableHeader =
    thead []
        [ tr []
            [ th [] [ text "Name" ]
            , th [] [ text "State" ]
            , th [] [ text "Actions" ]
            ]
        ]


kmTableBody : Html Msg
kmTableBody =
    tbody []
        [ kmTableRow
        , kmTableRow
        , kmTableRow
        ]


kmTableRow : Html Msg
kmTableRow =
    tr []
        [ td [] [ text "KM Name" ]
        , td [] [ text "Draft" ]
        , td [ class "table-actions" ] [ kmTableRowAction "Edit", kmTableRowAction "Upgrade" ]
        ]


kmTableRowAction : String -> Html Msg
kmTableRowAction name =
    a [ href "#" ]
        [ text name ]
