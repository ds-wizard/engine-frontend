module PackageManagement.Detail.View exposing (..)

import Common.Html exposing (linkTo)
import Common.View exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs exposing (Msg)
import PackageManagement.Detail.Models exposing (..)
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
                packageDetail model.packages
    in
    div []
        [ content ]


packageDetail : List PackageDetail -> Html Msgs.Msg
packageDetail packages =
    let
        ( name, shortName ) =
            case packages of
                first :: _ ->
                    ( first.name, first.shortName )

                _ ->
                    ( "", "" )
    in
    div [ class "col-xs-12 col-lg-10 col-lg-offset-1" ]
        [ pageHeader name actions
        , code [ class "package-short-name" ] [ text shortName ]
        , h3 [] [ text "Versions" ]
        , div [] (List.map versionView packages)
        ]


actions : List (Html Msgs.Msg)
actions =
    [ linkTo PackageManagement [ class "btn btn-default" ] [ text "Back" ]
    , button [ class "btn btn-danger" ] [ text "Delete" ]
    ]


versionView : PackageDetail -> Html Msgs.Msg
versionView detail =
    div [ class "panel panel-default panel-version" ]
        [ div [ class "panel-body" ]
            [ div [ class "labels" ]
                [ strong [] [ text detail.version ]
                , text detail.description
                ]
            , div [ class "actions" ]
                [ button [ class "btn btn-info link-with-icon" ] [ i [ class "fa fa-download" ] [], text "Export" ]
                , button [ class "btn btn-default" ] [ i [ class "fa fa-trash" ] [] ]
                ]
            ]
        ]
