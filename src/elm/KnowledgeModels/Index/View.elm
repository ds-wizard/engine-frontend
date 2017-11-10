module KnowledgeModels.Index.View exposing (..)

import Common.Html exposing (linkTo)
import Common.View exposing (defaultFullPageError, fullPageLoader, modalView, pageHeader)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import KnowledgeModels.Index.Models exposing (..)
import KnowledgeModels.Index.Msgs exposing (Msg(..))
import KnowledgeModels.Models exposing (KnowledgeModel)
import Msgs
import Routing


view : Model -> Html Msgs.Msg
view model =
    let
        content =
            if model.loading then
                fullPageLoader
            else if model.error /= "" then
                defaultFullPageError model.error
            else
                kmTable model
    in
    div []
        [ pageHeader "Knowledge model" indexActions
        , content
        , deleteModal model
        ]


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo Routing.KnowledgeModelsCreate
        [ class "btn btn-primary" ]
        [ text "Create KM" ]
    ]


kmTable : Model -> Html Msgs.Msg
kmTable model =
    table [ class "table" ]
        [ kmTableHeader
        , kmTableBody model
        ]


kmTableHeader : Html Msgs.Msg
kmTableHeader =
    thead []
        [ tr []
            [ th [] [ text "Name" ]
            , th [] [ text "Short Name" ]
            , th [] [ text "Parent KM" ]
            , th [] [ text "Actions" ]
            ]
        ]


kmTableBody : Model -> Html Msgs.Msg
kmTableBody model =
    if List.isEmpty model.knowledgeModels then
        kmTableEmpty
    else
        tbody [] (List.map kmTableRow model.knowledgeModels)


kmTableEmpty : Html msg
kmTableEmpty =
    tr []
        [ td [ colspan 4, class "td-empty-table" ] [ text "There are no knowledge models." ] ]


kmTableRow : KnowledgeModel -> Html Msgs.Msg
kmTableRow km =
    let
        parent =
            case ( km.parentPackageName, km.parentPackageVersion ) of
                ( Just name, Just version ) ->
                    name ++ ":" ++ version

                _ ->
                    "-"
    in
    tr []
        [ td [] [ text km.name ]
        , td [] [ text km.shortName ]
        , td [] [ text parent ]
        , td [ class "table-actions" ]
            [ kmTableRowAction "Edit"
            , kmTableRowAction "Upgrade"
            , kmTableRowActionDelete km
            ]
        ]


kmTableRowAction : String -> Html Msgs.Msg
kmTableRowAction name =
    a [ href "#" ]
        [ text name ]


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
            , actionActive = model.deletingKM
            , actionName = "Delete"
            , actionError = model.deleteKMError
            , actionMsg = Msgs.KnowledgeModelsIndexMsg DeleteKnowledgeModel
            , cancelMsg = Msgs.KnowledgeModelsIndexMsg <| ShowHideDeleteKnowledgeModel Nothing
            }
    in
    modalView modalConfig
