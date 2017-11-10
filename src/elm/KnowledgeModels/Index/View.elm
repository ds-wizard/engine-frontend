module KnowledgeModels.Index.View exposing (..)

import Common.Html exposing (linkTo)
import Common.View exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Html exposing (..)
import Html.Attributes exposing (..)
import KnowledgeModels.Index.Models exposing (..)
import KnowledgeModels.Models exposing (KnowledgeModel)
import Msgs exposing (Msg)
import Routing


view : Model -> Html Msg
view model =
    let
        content =
            if model.loading then
                fullPageLoader
            else if model.error /= "" then
                defaultFullPageError model.error
            else
                kmTable model
    in
    div []
        [ pageHeader "Knowledge model" indexActions
        , content
        ]


indexActions : List (Html Msg)
indexActions =
    [ linkTo Routing.KnowledgeModelsCreate
        [ class "btn btn-primary" ]
        [ text "Create KM" ]
    ]


kmTable : Model -> Html Msg
kmTable model =
    table [ class "table" ]
        [ kmTableHeader
        , kmTableBody model
        ]


kmTableHeader : Html Msg
kmTableHeader =
    thead []
        [ tr []
            [ th [] [ text "Name" ]
            , th [] [ text "Short Name" ]
            , th [] [ text "Parent KM" ]
            , th [] [ text "Actions" ]
            ]
        ]


kmTableBody : Model -> Html Msg
kmTableBody model =
    if List.isEmpty model.knowledgeModels then
        kmTableEmpty
    else
        tbody [] (List.map kmTableRow model.knowledgeModels)


kmTableEmpty : Html msg
kmTableEmpty =
    tr []
        [ td [ colspan 4, class "td-empty-table" ] [ text "There are no knowledge models." ] ]


kmTableRow : KnowledgeModel -> Html Msg
kmTableRow km =
    let
        parent =
            case ( km.parentPackageName, km.parentPackageVersion ) of
                ( Just name, Just version ) ->
                    name ++ ":" ++ version

                _ ->
                    "-"
    in
    tr []
        [ td [] [ text km.name ]
        , td [] [ text km.shortName ]
        , td [] [ text parent ]
        , td [ class "table-actions" ] [ kmTableRowAction "Edit", kmTableRowAction "Upgrade" ]
        ]


kmTableRowAction : String -> Html Msg
kmTableRowAction name =
    a [ href "#" ]
        [ text name ]
