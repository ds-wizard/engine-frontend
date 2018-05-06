module PackageManagement.Index.View exposing (view)

{-|

@docs view

-}

import Common.Html exposing (..)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs exposing (Msg)
import PackageManagement.Index.Models exposing (..)
import PackageManagement.Models exposing (..)
import Routing exposing (Route(..))


{-| -}
view : Model -> Html Msgs.Msg
view model =
    div []
        [ pageHeader "Knowledge Model Packages" actions
        , content model
        ]


content : Model -> Html Msgs.Msg
content model =
    case model.packages of
        Unset ->
            emptyNode

        Loading ->
            fullPageLoader

        Error err ->
            defaultFullPageError err

        Success packages ->
            pmTable packages


actions : List (Html Msg)
actions =
    [ linkTo PackageManagementImport
        [ class "btn btn-info link-with-icon" ]
        [ i [ class "fa fa-cloud-upload" ] []
        , text "Import"
        ]
    ]


pmTable : List Package -> Html Msg
pmTable packages =
    table [ class "table" ]
        [ pmTableHeader
        , pmTableBody packages
        ]


pmTableHeader : Html Msg
pmTableHeader =
    thead []
        [ tr []
            [ th [] [ text "Name" ]
            , th [] [ text "Organization ID" ]
            , th [] [ text "Knowledge Model ID" ]
            ]
        ]


pmTableBody : List Package -> Html Msg
pmTableBody packages =
    if List.isEmpty packages then
        pmTableEmpty
    else
        tbody [] (List.map pmTableRow packages)


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
