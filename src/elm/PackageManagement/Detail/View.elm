module PackageManagement.Detail.View exposing (..)

import Common.Html exposing (linkTo)
import Common.View exposing (defaultFullPageError, fullPageLoader, modalView, pageHeader)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Msgs exposing (Msg)
import PackageManagement.Detail.Models exposing (..)
import PackageManagement.Detail.Msgs exposing (..)
import PackageManagement.Models exposing (..)
import Routing exposing (Route(..))
import Tuple exposing (first)


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
        [ content
        , deleteModal model
        , deleteVersionModal model
        ]


deleteModal : Model -> Html Msgs.Msg
deleteModal model =
    let
        modalContent =
            [ p []
                [ text "Are you sure you want to permanently delete "
                , strong [] [ text (model.packages |> getPackageName |> first) ]
                , text " and all its versions?"
                ]
            ]

        modalConfig =
            { modalTitle = "Delete package"
            , modalContent = modalContent
            , visible = model.showDeleteDialog
            , actionActive = model.deletingPackage
            , actionName = "Delete"
            , actionError = model.deleteError
            , actionMsg = Msgs.PackageManagementDetailMsg DeletePackage
            , cancelMsg = Msgs.PackageManagementDetailMsg <| ShowHideDeleteDialog False
            }
    in
    modalView modalConfig


deleteVersionModal : Model -> Html Msgs.Msg
deleteVersionModal model =
    let
        modalContent =
            [ p []
                [ text "Are you sure you want to permanently delete version "
                , strong [] [ text model.versionToBeDeleted ]
                , text "?"
                ]
            ]

        modalConfig =
            { modalTitle = "Delete version"
            , modalContent = modalContent
            , visible = model.versionToBeDeleted /= ""
            , actionActive = model.deletingVersion
            , actionName = "Delete"
            , actionError = model.deleteVersionError
            , actionMsg = Msgs.PackageManagementDetailMsg DeleteVersion
            , cancelMsg = Msgs.PackageManagementDetailMsg <| ShowHideDeleteVersion ""
            }
    in
    modalView modalConfig


packageDetail : List PackageDetail -> Html Msgs.Msg
packageDetail packages =
    let
        ( name, shortName ) =
            getPackageName packages
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
    , button
        [ onClick (Msgs.PackageManagementDetailMsg <| ShowHideDeleteDialog True)
        , class "btn btn-danger"
        ]
        [ text "Delete" ]
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
                , button
                    [ onClick (Msgs.PackageManagementDetailMsg <| ShowHideDeleteVersion detail.version)
                    , class "btn btn-default"
                    ]
                    [ i [ class "fa fa-trash" ] [] ]
                ]
            ]
        ]
