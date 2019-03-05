module KMPackages.Detail.View exposing (view)

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Common.Html exposing (emptyNode)
import Common.Html.Attribute exposing (detailClass, linkToAttributes)
import Common.View.FormGroup as FormGroup
import Common.View.Modal as Modal
import Common.View.Page as Page
import DSPlanner.Routing
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import KMEditor.Routing
import KMPackages.Detail.Models exposing (..)
import KMPackages.Detail.Msgs exposing (..)
import KMPackages.Requests exposing (exportPackageUrl)
import Msgs
import Routing


view : (Msg -> Msgs.Msg) -> Maybe JwtToken -> Model -> Html Msgs.Msg
view wrapMsg jwt model =
    div [ detailClass "KMPackages__Detail" ]
        [ Page.actionResultView (packageDetail wrapMsg jwt) model.packages
        , deleteVersionModal wrapMsg model
        ]


packageDetail : (Msg -> Msgs.Msg) -> Maybe JwtToken -> List PackageDetailRow -> Html Msgs.Msg
packageDetail wrapMsg jwt packages =
    case List.head packages of
        Just package ->
            div []
                [ Page.header package.packageDetail.name []
                , FormGroup.codeView package.packageDetail.organizationId "Organization ID"
                , FormGroup.codeView package.packageDetail.kmId "Knowledge Model ID"
                , FormGroup.codeView (String.fromInt package.packageDetail.metamodelVersion) "Metamodel Version"
                , h3 [] [ text "Versions" ]
                , div [] (List.map (versionView wrapMsg jwt) packages)
                ]

        Nothing ->
            emptyNode


versionView : (Msg -> Msgs.Msg) -> Maybe JwtToken -> PackageDetailRow -> Html Msgs.Msg
versionView wrapMsg jwt row =
    div [ class "card bg-light mb-3" ]
        [ div [ class "card-body row" ]
            [ div [ class "col-4 labels" ]
                [ strong [] [ text row.packageDetail.version ] ]
            , div [ class "col-8 text-right actions" ]
                [ versionViewActions wrapMsg jwt row ]
            , div [ class "col-12" ] [ text row.packageDetail.description ]
            ]
        ]


versionViewActions : (Msg -> Msgs.Msg) -> Maybe JwtToken -> PackageDetailRow -> Html Msgs.Msg
versionViewActions wrapMsg jwt row =
    let
        id =
            row.packageDetail.id

        exportAndDeleteButtons =
            if hasPerm jwt Perm.packageManagementWrite then
                [ a [ class "btn btn-outline-primary link-with-icon", href <| exportPackageUrl id, target "_blank" ]
                    [ i [ class "fa fa-download" ] [], text "Export" ]
                , a [ class "btn btn-outline-primary", onClick (wrapMsg <| ShowHideDeleteVersion <| Just id) ]
                    [ i [ class "fa fa-trash-o" ] [] ]
                ]

            else
                []

        forkKMItem =
            if hasPerm jwt Perm.knowledgeModel then
                [ Dropdown.anchorItem
                    (linkToAttributes (Routing.KMEditor <| KMEditor.Routing.CreateRoute <| Just id))
                    [ text "Fork Knowledge Model" ]
                ]

            else
                []

        createQuestionnaireItem =
            if hasPerm jwt Perm.questionnaire then
                [ Dropdown.anchorItem
                    (linkToAttributes (Routing.DSPlanner <| DSPlanner.Routing.Create <| Just id))
                    [ text "Create Questionnaire" ]
                ]

            else
                []

        items =
            forkKMItem ++ createQuestionnaireItem

        dropdown =
            if List.length items > 0 then
                [ Dropdown.dropdown row.dropdownState
                    { options = [ Dropdown.alignMenuRight ]
                    , toggleMsg = wrapMsg << DropdownMsg row.packageDetail
                    , toggleButton = Dropdown.toggle [ Button.outlinePrimary ] []
                    , items = items
                    }
                ]

            else
                []
    in
    div [ class "btn-group" ]
        (exportAndDeleteButtons ++ dropdown)


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
    Modal.confirm modalConfig
