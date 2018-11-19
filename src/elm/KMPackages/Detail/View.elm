module KMPackages.Detail.View exposing (view)

import Auth.Models exposing (JwtToken)
import Auth.Permission exposing (hasPerm, packageManagementWrite)
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Common.Html exposing (detailContainerClassWith, emptyNode, linkTo, linkToAttributes)
import Common.View exposing (defaultFullPageError, fullPageActionResultView, fullPageLoader, modalView, pageHeader)
import Common.View.Forms exposing (codeGroup)
import DSPlanner.Routing
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import KMEditor.Routing
import KMPackages.Common.Models exposing (..)
import KMPackages.Detail.Models exposing (..)
import KMPackages.Detail.Msgs exposing (..)
import KMPackages.Requests exposing (exportPackageUrl)
import Msgs
import Routing


view : (Msg -> Msgs.Msg) -> Maybe JwtToken -> Model -> Html Msgs.Msg
view wrapMsg jwt model =
    div [ detailContainerClassWith "KMPackages__Detail" ]
        [ fullPageActionResultView (packageDetail wrapMsg jwt) model.packages
        , deleteVersionModal wrapMsg model
        ]


packageDetail : (Msg -> Msgs.Msg) -> Maybe JwtToken -> List PackageDetailRow -> Html Msgs.Msg
packageDetail wrapMsg jwt packages =
    case List.head packages of
        Just package ->
            div []
                [ pageHeader package.packageDetail.name []
                , codeGroup package.packageDetail.organizationId "Organization ID"
                , codeGroup package.packageDetail.kmId "Knowledge Model ID"
                , h3 [] [ text "Versions" ]
                , div [] (List.map (versionView wrapMsg jwt) packages)
                ]

        Nothing ->
            emptyNode


versionView : (Msg -> Msgs.Msg) -> Maybe JwtToken -> PackageDetailRow -> Html Msgs.Msg
versionView wrapMsg jwt row =
    let
        actions =
            if hasPerm jwt packageManagementWrite then
                versionViewActions wrapMsg row

            else
                versionViewActionsJustCreateDSButton row.packageDetail
    in
    div [ class "card bg-light mb-3" ]
        [ div [ class "card-body row" ]
            [ div [ class "col-4 labels" ]
                [ strong [] [ text row.packageDetail.version ] ]
            , div [ class "col-8 text-right actions" ]
                [ actions ]
            , div [ class "col-12" ] [ text row.packageDetail.description ]
            ]
        ]


versionViewActions : (Msg -> Msgs.Msg) -> PackageDetailRow -> Html Msgs.Msg
versionViewActions wrapMsg row =
    let
        id =
            row.packageDetail.id

        url =
            exportPackageUrl id
    in
    div [ class "btn-group" ]
        [ a [ class "btn btn-outline-primary link-with-icon", href url, target "_blank" ]
            [ i [ class "fa fa-download" ] [], text "Export" ]
        , a [ class "btn btn-outline-primary", onClick (wrapMsg <| ShowHideDeleteVersion <| Just id) ]
            [ i [ class "fa fa-trash-o" ] [] ]
        , Dropdown.dropdown row.dropdownState
            { options = [ Dropdown.alignMenuRight ]
            , toggleMsg = wrapMsg << DropdownMsg row.packageDetail
            , toggleButton = Dropdown.toggle [ Button.outlinePrimary ] []
            , items =
                [ Dropdown.anchorItem
                    (linkToAttributes (Routing.KMEditor <| KMEditor.Routing.Create <| Just id))
                    [ text "Create KM Editor" ]
                , Dropdown.anchorItem
                    (linkToAttributes (Routing.DSPlanner <| DSPlanner.Routing.Create <| Just id))
                    [ text "Create DS Planner" ]
                ]
            }
        ]


versionViewActionsJustCreateDSButton : PackageDetail -> Html Msgs.Msg
versionViewActionsJustCreateDSButton detail =
    linkTo (Routing.DSPlanner <| DSPlanner.Routing.Create <| Just detail.id)
        [ class "btn btn-outline-primary" ]
        [ text "Create DS Planner" ]


deleteVersionModal : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
deleteVersionModal wrapMsg model =
    let
        ( version, visible ) =
            case model.versionToBeDeleted of
                Just value ->
                    ( value, True )

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
            , cancelMsg = Just <| wrapMsg <| ShowHideDeleteVersion Nothing
            }
    in
    modalView modalConfig
