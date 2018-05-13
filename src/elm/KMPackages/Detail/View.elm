module KMPackages.Detail.View exposing (view)

import Common.Html exposing (detailContainerClassWith, emptyNode, linkTo)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, modalView, pageHeader)
import Common.View.Forms exposing (codeGroup)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import KMPackages.Detail.Models exposing (..)
import KMPackages.Detail.Msgs exposing (..)
import KMPackages.Models exposing (..)
import KMPackages.Requests exposing (exportPackageUrl)
import Msgs exposing (Msg)


view : Model -> Html Msgs.Msg
view model =
    div [ detailContainerClassWith "package-management-detail" ]
        [ content model
        , deleteVersionModal model
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
            packageDetail packages


deleteVersionModal : Model -> Html Msgs.Msg
deleteVersionModal model =
    let
        ( version, visible ) =
            case model.versionToBeDeleted of
                Just version ->
                    ( version, True )

                Nothing ->
                    ( "", False )

        modalContent =
            [ p []
                [ text "Are you sure you want to permanently delete version "
                , strong [] [ text version ]
                , text "?"
                ]
            ]

        modalConfig =
            { modalTitle = "Delete version"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingVersion
            , actionName = "Delete"
            , actionMsg = Msgs.PackageManagementDetailMsg DeleteVersion
            , cancelMsg = Msgs.PackageManagementDetailMsg <| ShowHideDeleteVersion Nothing
            }
    in
    modalView modalConfig


packageDetail : List PackageDetail -> Html Msgs.Msg
packageDetail packages =
    case List.head packages of
        Just package ->
            div []
                [ pageHeader package.name []
                , codeGroup package.organizationId "Organization ID"
                , codeGroup package.kmId "Knowledge Model ID"
                , h3 [] [ text "Versions" ]
                , div [] (List.map versionView packages)
                ]

        Nothing ->
            text ""


versionView : PackageDetail -> Html Msgs.Msg
versionView detail =
    let
        url =
            exportPackageUrl detail.id
    in
    div [ class "panel panel-default panel-version" ]
        [ div [ class "panel-body" ]
            [ div [ class "labels" ]
                [ strong [] [ text detail.version ]
                , text detail.description
                ]
            , div [ class "actions" ]
                [ a [ class "link-with-icon", href url, target "_blank" ] [ i [ class "fa fa-download" ] [], text "Export" ]
                , a
                    [ onClick (Msgs.PackageManagementDetailMsg <| ShowHideDeleteVersion <| Just detail.id)
                    ]
                    [ i [ class "fa fa-trash-o" ] [] ]
                ]
            ]
        ]
