module KnowledgeModels.Index.View exposing (..)

import Common.Html exposing (..)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, modalView, pageHeader)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import KnowledgeModels.Index.Models exposing (..)
import KnowledgeModels.Index.Msgs exposing (Msg(..))
import KnowledgeModels.Models exposing (KnowledgeModel)
import Msgs
import Routing exposing (Route(..))


view : Model -> Html Msgs.Msg
view model =
    div []
        [ pageHeader "Knowledge model" indexActions
        , content model
        , deleteModal model
        ]


content : Model -> Html Msgs.Msg
content model =
    case model.knowledgeModels of
        Unset ->
            emptyNode

        Loading ->
            fullPageLoader

        Error err ->
            defaultFullPageError err

        Success knowledgeModels ->
            kmTable knowledgeModels


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo Routing.KnowledgeModelsCreate
        [ class "btn btn-primary" ]
        [ text "Create KM" ]
    ]


kmTable : List KnowledgeModel -> Html Msgs.Msg
kmTable knowledgeModels =
    table [ class "table" ]
        [ kmTableHeader
        , kmTableBody knowledgeModels
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


kmTableBody : List KnowledgeModel -> Html Msgs.Msg
kmTableBody knowledgeModels =
    if List.isEmpty knowledgeModels then
        kmTableEmpty
    else
        tbody [] (List.map kmTableRow knowledgeModels)


kmTableEmpty : Html msg
kmTableEmpty =
    tr []
        [ td [ colspan 4, class "td-empty-table" ] [ text "There are no knowledge models." ] ]


kmTableRow : KnowledgeModel -> Html Msgs.Msg
kmTableRow km =
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
            [ kmTableRowAction "Edit"
            , kmTableRowAction "Upgrade"
            , kmTableRowActionPublish km
            , kmTableRowActionDelete km
            ]
        ]


kmTableRowAction : String -> Html Msgs.Msg
kmTableRowAction name =
    a [ href "#" ]
        [ text name ]


kmTableRowActionPublish : KnowledgeModel -> Html Msgs.Msg
kmTableRowActionPublish km =
    linkTo (KnowledgeModelsPublish km.uuid) [] [ text "Publish" ]


kmTableRowActionDelete : KnowledgeModel -> Html Msgs.Msg
kmTableRowActionDelete km =
    a [ onClick <| Msgs.KnowledgeModelsIndexMsg <| ShowHideDeleteKnowledgeModel <| Just km ]
        [ text "Delete" ]


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
