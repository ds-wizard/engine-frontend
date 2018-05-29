module KMPackages.Detail.View exposing (view)

import Common.Html exposing (detailContainerClassWith, emptyNode, linkTo)
import Common.View exposing (defaultFullPageError, fullPageActionResultView, fullPageLoader, modalView, pageHeader)
import Common.View.Forms exposing (codeGroup)
import DSPlanner.Routing
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import KMPackages.Common.Models exposing (..)
import KMPackages.Detail.Models exposing (..)
import KMPackages.Detail.Msgs exposing (..)
import KMPackages.Requests exposing (exportPackageUrl)
import Msgs
import Routing


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ detailContainerClassWith "KMPackages__Detail" ]
        [ fullPageActionResultView (packageDetail wrapMsg) model.packages
        , deleteVersionModal wrapMsg model
        ]


packageDetail : (Msg -> Msgs.Msg) -> List PackageDetail -> Html Msgs.Msg
packageDetail wrapMsg packages =
    case List.head packages of
        Just package ->
            div []
                [ pageHeader package.name []
                , codeGroup package.organizationId "Organization ID"
                , codeGroup package.kmId "Knowledge Model ID"
                , h3 [] [ text "Versions" ]
                , div [] (List.map (versionView wrapMsg) packages)
                ]

        Nothing ->
            emptyNode


versionView : (Msg -> Msgs.Msg) -> PackageDetail -> Html Msgs.Msg
versionView wrapMsg detail =
    let
        url =
            exportPackageUrl detail.id
    in
    div [ class "panel panel-default" ]
        [ div [ class "panel-body" ]
            [ div [ class "labels" ]
                [ strong [] [ text detail.version ]
                , text detail.description
                ]
            , div [ class "actions" ]
                [ linkTo (Routing.KMEditorCreate <| Just detail.id)
                    []
                    [ text "Create KM Editor" ]
                , linkTo (Routing.DSPlanner <| DSPlanner.Routing.Create <| Just detail.id)
                    []
                    [ text "Create DS Planner" ]
                , a [ class "link-with-icon", href url, target "_blank" ] [ i [ class "fa fa-download" ] [], text "Export" ]
                , a
                    [ onClick (wrapMsg <| ShowHideDeleteVersion <| Just detail.id) ]
                    [ i [ class "fa fa-trash-o" ] [] ]
                ]
            ]
        ]


deleteVersionModal : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
deleteVersionModal wrapMsg model =
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
            , actionMsg = wrapMsg DeleteVersion
            , cancelMsg = wrapMsg <| ShowHideDeleteVersion Nothing
            }
    in
    modalView modalConfig
