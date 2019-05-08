module KnowledgeModels.Detail.View exposing (view)

import Auth.Permission as Perm exposing (hasPerm)
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Common.Api.Packages exposing (exportPackageUrl)
import Common.AppState exposing (AppState)
import Common.Html.Attribute exposing (detailClass, linkToAttributes)
import Common.View.FormGroup as FormGroup
import Common.View.Modal as Modal
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import KMEditor.Routing
import KnowledgeModels.Detail.Models exposing (..)
import KnowledgeModels.Detail.Msgs exposing (..)
import Msgs
import Questionnaires.Routing
import Routing


view : (Msg -> Msgs.Msg) -> AppState -> Model -> Html Msgs.Msg
view wrapMsg appState model =
    Page.actionResultView (packageDetail wrapMsg appState model) model.packages


packageDetail : (Msg -> Msgs.Msg) -> AppState -> Model -> List PackageDetailRow -> Html Msgs.Msg
packageDetail wrapMsg appState model packages =
    case List.head packages of
        Just package ->
            div [ detailClass "KnowledgeModels__Detail" ]
                [ div []
                    [ Page.header package.packageDetail.name []
                    , FormGroup.codeView package.packageDetail.organizationId "Organization ID"
                    , FormGroup.codeView package.packageDetail.kmId "Knowledge Model ID"
                    , FormGroup.codeView (String.fromInt package.packageDetail.metamodelVersion) "Metamodel Version"
                    , h3 [] [ text "Versions" ]
                    , div [] (List.map (versionView wrapMsg appState) <| sortPackageDetailRowsByVersion packages)
                    ]
                , deleteVersionModal wrapMsg model
                ]

        Nothing ->
            Page.error "Empty knowledge model list returned."


versionView : (Msg -> Msgs.Msg) -> AppState -> PackageDetailRow -> Html Msgs.Msg
versionView wrapMsg appState row =
    div [ class "card bg-light mb-3" ]
        [ div [ class "card-body" ]
            [ div [ class "row align-items-center" ]
                [ div [ class "col-4 labels" ]
                    [ strong [] [ text row.packageDetail.version ] ]
                , div [ class "col-8 text-right actions" ]
                    [ versionViewActions wrapMsg appState row ]
                ]
            , div [ class "row mt-3" ]
                [ div [ class "col-12" ] [ text row.packageDetail.description ]
                ]
            ]
        ]


versionViewActions : (Msg -> Msgs.Msg) -> AppState -> PackageDetailRow -> Html Msgs.Msg
versionViewActions wrapMsg appState row =
    let
        id =
            row.packageDetail.id

        exportAndDeleteButtons =
            if hasPerm appState.jwt Perm.packageManagementWrite then
                [ a [ class "btn btn-outline-primary link-with-icon", href <| exportPackageUrl id appState, target "_blank" ]
                    [ i [ class "fa fa-download" ] [], text "Export" ]
                , a [ class "btn btn-outline-primary", onClick (wrapMsg <| ShowHideDeleteVersion <| Just id) ]
                    [ i [ class "fa fa-trash-o" ] [] ]
                ]

            else
                []

        forkKMItem =
            if hasPerm appState.jwt Perm.knowledgeModel then
                [ Dropdown.anchorItem
                    (linkToAttributes (Routing.KMEditor <| KMEditor.Routing.CreateRoute <| Just id))
                    [ text "Fork Knowledge Model" ]
                ]

            else
                []

        createQuestionnaireItem =
            if hasPerm appState.jwt Perm.questionnaire then
                [ Dropdown.anchorItem
                    (linkToAttributes (Routing.Questionnaires <| Questionnaires.Routing.Create <| Just id))
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
