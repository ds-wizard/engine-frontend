module PackageManagement.Index.View exposing (..)

import Common.Html exposing (linkTo)
import Common.View exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs exposing (Msg)
import PackageManagement.Index.Models exposing (..)
import PackageManagement.Models exposing (..)
import Routing exposing (Route(..))


view : Model -> Html Msgs.Msg
view model =
    let
        content =
            if model.loading then
                fullPageLoader
            else if model.error /= "" then
                defaultFullPageError model.error
            else
                pmTable model
    in
    div []
        [ pageHeader "Package Management" actions
        , content
        ]


actions : List (Html Msg)
actions =
    [ linkTo PackageManagementImport
        [ class "btn btn-info link-with-icon" ]
        [ i [ class "fa fa-upload" ] []
        , text "Import"
        ]
    ]


pmTable : Model -> Html Msg
pmTable model =
    table [ class "table" ]
        [ pmTableHeader
        , pmTableBody model
        ]


pmTableHeader : Html Msg
pmTableHeader =
    thead []
        [ tr []
            [ th [] [ text "Name" ]
            , th [] [ text "Group ID" ]
            , th [] [ text "Artifact ID" ]
            ]
        ]


pmTableBody : Model -> Html Msg
pmTableBody model =
    if List.isEmpty model.packages then
        pmTableEmpty
    else
        tbody [] (List.map pmTableRow model.packages)


pmTableEmpty : Html msg
pmTableEmpty =
    tr []
        [ td [ colspan 3, class "td-empty-table" ] [ text "There are no packages." ] ]


pmTableRow : Package -> Html Msg
pmTableRow package =
    tr []
        [ td [] [ linkTo (PackageManagementDetail package.groupId package.artifactId) [] [ text package.name ] ]
        , td [] [ text package.groupId ]
        , td [] [ text package.artifactId ]
        ]
