module KMPackages.Index.View exposing (view)

{-|

@docs view

-}

import Common.Html exposing (..)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Html exposing (..)
import Html.Attributes exposing (..)
import KMPackages.Index.Models exposing (..)
import KMPackages.Models exposing (..)
import Msgs exposing (Msg)
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
    [ linkTo KMPackagesImport
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
        [ td [] [ linkTo (KMPackagesDetail package.organizationId package.kmId) [] [ text package.name ] ]
        , td [] [ text package.organizationId ]
        , td [] [ text package.kmId ]
        ]
