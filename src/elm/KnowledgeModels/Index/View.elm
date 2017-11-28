module KnowledgeModels.Index.View exposing (..)

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import Common.Html exposing (..)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, modalView, pageHeader)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import KnowledgeModels.Index.Models exposing (..)
import KnowledgeModels.Index.Msgs exposing (Msg(..))
import KnowledgeModels.Models exposing (KnowledgeModel, KnowledgeModelState(..), kmMatchState)
import Msgs
import Routing exposing (Route(..))


view : Maybe JwtToken -> Model -> Html Msgs.Msg
view jwt model =
    div []
        [ pageHeader "Knowledge model" indexActions
        , content jwt model
        , deleteModal model
        ]


content : Maybe JwtToken -> Model -> Html Msgs.Msg
content jwt model =
    case model.knowledgeModels of
        Unset ->
            emptyNode

        Loading ->
            fullPageLoader

        Error err ->
            defaultFullPageError err

        Success knowledgeModels ->
            kmTable jwt knowledgeModels


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo Routing.KnowledgeModelsCreate
        [ class "btn btn-primary" ]
        [ text "Create KM" ]
    ]


kmTable : Maybe JwtToken -> List KnowledgeModel -> Html Msgs.Msg
kmTable jwt knowledgeModels =
    table [ class "table" ]
        [ kmTableHeader
        , kmTableBody jwt knowledgeModels
        ]


kmTableHeader : Html Msgs.Msg
kmTableHeader =
    thead []
        [ tr []
            [ th [] [ text "Name" ]
            , th [] [ text "Artifact ID" ]
            , th [] [ text "Parent KM" ]
            , th [] [ text "Actions" ]
            ]
        ]


kmTableBody : Maybe JwtToken -> List KnowledgeModel -> Html Msgs.Msg
kmTableBody jwt knowledgeModels =
    if List.isEmpty knowledgeModels then
        kmTableEmpty
    else
        tbody [] (List.map (kmTableRow jwt) knowledgeModels)


kmTableEmpty : Html msg
kmTableEmpty =
    tr []
        [ td [ colspan 4, class "td-empty-table" ] [ text "There are no knowledge models." ] ]


kmTableRow : Maybe JwtToken -> KnowledgeModel -> Html Msgs.Msg
kmTableRow jwt km =
    let
        parent =
            case km.parentPackageId of
                Just parentPackageId ->
                    parentPackageId

                _ ->
                    "-"
    in
    tr []
        [ td [] [ text km.name ]
        , td [] [ text km.artifactId ]
        , td [] [ text parent ]
        , td [ class "table-actions" ]
            [ kmTableRowActionDelete km
            , kmTableRowActionEdit km
            , kmTableRowActionPublish jwt km
            , kmTableRowActionUpgrade jwt km
            , kmTableRowActionContinueMigration jwt km
            ]
        ]


kmTableRowAction : String -> Html Msgs.Msg
kmTableRowAction name =
    a [ href "#" ]
        [ text name ]


kmTableRowActionEdit : KnowledgeModel -> Html Msgs.Msg
kmTableRowActionEdit km =
    if kmMatchState [ Default, Edited, Outdated ] km then
        linkTo (KnowledgeModelsEditor km.uuid) [] [ i [ class "fa fa-edit" ] [] ]
    else
        emptyNode


kmTableRowActionPublish : Maybe JwtToken -> KnowledgeModel -> Html Msgs.Msg
kmTableRowActionPublish jwt km =
    if hasPerm jwt Perm.knowledgeModelPublish && kmMatchState [ Edited ] km then
        linkTo (KnowledgeModelsPublish km.uuid)
            []
            [ text "Publish"
            ]
    else
        emptyNode


kmTableRowActionUpgrade : Maybe JwtToken -> KnowledgeModel -> Html Msgs.Msg
kmTableRowActionUpgrade jwt km =
    if hasPerm jwt Perm.knowledgeModelUpgrade && kmMatchState [ Outdated ] km then
        a []
            [ text "Upgrade"
            ]
    else
        emptyNode


kmTableRowActionContinueMigration : Maybe JwtToken -> KnowledgeModel -> Html Msgs.Msg
kmTableRowActionContinueMigration jwt km =
    if hasPerm jwt Perm.knowledgeModelUpgrade && kmMatchState [ Migrating ] km then
        linkTo (KnowledgeModelsMigration km.uuid)
            []
            [ text "Continue Migration"
            ]
    else
        emptyNode


kmTableRowActionDelete : KnowledgeModel -> Html Msgs.Msg
kmTableRowActionDelete km =
    a [ onClick <| Msgs.KnowledgeModelsIndexMsg <| ShowHideDeleteKnowledgeModel <| Just km ]
        [ i [ class "fa fa-trash-o" ] [] ]


deleteModal : Model -> Html Msgs.Msg
deleteModal model =
    let
        ( visible, name ) =
            case model.kmToBeDeleted of
                Just km ->
                    ( True, km.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                [ text "Are you sure you want to permanently delete "
                , strong [] [ text name ]
                , text "?"
                ]
            ]

        modalConfig =
            { modalTitle = "Delete knowledge model"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingKnowledgeModel
            , actionName = "Delete"
            , actionMsg = Msgs.KnowledgeModelsIndexMsg DeleteKnowledgeModel
            , cancelMsg = Msgs.KnowledgeModelsIndexMsg <| ShowHideDeleteKnowledgeModel Nothing
            }
    in
    modalView modalConfig
