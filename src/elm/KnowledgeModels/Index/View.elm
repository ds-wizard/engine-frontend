module KnowledgeModels.Index.View exposing (view)

{-|

@docs view

-}

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import Common.Html exposing (..)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, modalView, pageHeader)
import Common.View.Forms exposing (formResultView, selectGroup)
import Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import KnowledgeModels.Index.Models exposing (..)
import KnowledgeModels.Index.Msgs exposing (Msg(..))
import KnowledgeModels.Models exposing (KnowledgeModel, KnowledgeModelState(..), kmMatchState)
import Msgs
import PackageManagement.Models exposing (PackageDetail)
import Routing exposing (Route(..))


{-| -}
view : Maybe JwtToken -> Model -> Html Msgs.Msg
view jwt model =
    div []
        [ pageHeader "Knowledge Model Editor" indexActions
        , formResultView model.deletingMigration
        , content jwt model
        , deleteModal model
        , upgradeModal model
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
            kmTable jwt model knowledgeModels


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo Routing.KnowledgeModelsCreate
        [ class "btn btn-primary" ]
        [ text "Create" ]
    ]


kmTable : Maybe JwtToken -> Model -> List KnowledgeModel -> Html Msgs.Msg
kmTable jwt model knowledgeModels =
    table [ class "table" ]
        [ kmTableHeader
        , kmTableBody jwt model knowledgeModels
        ]


kmTableHeader : Html Msgs.Msg
kmTableHeader =
    thead []
        [ tr []
            [ th [] [ text "Name" ]
            , th [] [ text "Knowledge Model ID" ]
            , th [] [ text "Parent Package ID" ]
            , th [] [ text "Actions" ]
            ]
        ]


kmTableBody : Maybe JwtToken -> Model -> List KnowledgeModel -> Html Msgs.Msg
kmTableBody jwt model knowledgeModels =
    if List.isEmpty knowledgeModels then
        kmTableEmpty
    else
        tbody [] (List.map (kmTableRow jwt model) knowledgeModels)


kmTableEmpty : Html msg
kmTableEmpty =
    tr []
        [ td [ colspan 4, class "td-empty-table" ] [ text "There are no knowledge models." ] ]


kmTableRow : Maybe JwtToken -> Model -> KnowledgeModel -> Html Msgs.Msg
kmTableRow jwt model km =
    tr []
        [ td [ class "td-with-labels" ] [ kmTableRowName km ]
        , td [] [ text km.kmId ]
        , td [] [ text (Maybe.withDefault "-" km.lastAppliedParentPackageId) ]
        , td [ class "table-actions" ]
            [ kmTableRowActionDelete km
            , kmTableRowActionEdit km
            , kmTableRowActionPublish jwt km
            , kmTableRowActionUpgrade jwt model km
            , kmTableRowActionContinueMigration jwt km
            , kmTableRowActionCancelMigration jwt model km
            ]
        ]


kmTableRowName : KnowledgeModel -> Html Msgs.Msg
kmTableRowName km =
    let
        extra =
            case km.stateType of
                Outdated ->
                    span [ class "label label-warning" ]
                        [ text "outdated" ]

                Migrating ->
                    span [ class "label label-info" ]
                        [ text "migrating" ]

                Migrated ->
                    span [ class "label label-success" ]
                        [ text "migrated" ]

                Edited ->
                    i [ class "fa fa-pencil" ] []

                _ ->
                    emptyNode
    in
    span []
        [ text km.name
        , extra
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
    if hasPerm jwt Perm.knowledgeModelPublish && kmMatchState [ Edited, Migrated ] km then
        linkTo (KnowledgeModelsPublish km.uuid)
            []
            [ text "Publish"
            ]
    else
        emptyNode


kmTableRowActionUpgrade : Maybe JwtToken -> Model -> KnowledgeModel -> Html Msgs.Msg
kmTableRowActionUpgrade jwt model km =
    if hasPerm jwt Perm.knowledgeModelUpgrade && kmMatchState [ Outdated ] km then
        a [ onClick <| Msgs.KnowledgeModelsIndexMsg <| ShowHideUpgradeModal <| Just km ]
            [ text "Upgrade" ]
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


kmTableRowActionCancelMigration : Maybe JwtToken -> Model -> KnowledgeModel -> Html Msgs.Msg
kmTableRowActionCancelMigration jwt model km =
    if hasPerm jwt Perm.knowledgeModelUpgrade && kmMatchState [ Migrating, Migrated ] km then
        case model.deletingMigration of
            Loading ->
                a [ class "disabled" ] [ text "Cancel Migration" ]

            _ ->
                a [ onClick <| Msgs.KnowledgeModelsIndexMsg <| DeleteMigration km.uuid ]
                    [ text "Cancel Migration"
                    ]
    else
        emptyNode


kmTableRowActionDelete : KnowledgeModel -> Html Msgs.Msg
kmTableRowActionDelete km =
    a [ onClick <| Msgs.KnowledgeModelsIndexMsg <| ShowHideDeleteKnowledgeModal <| Just km ]
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
            , cancelMsg = Msgs.KnowledgeModelsIndexMsg <| ShowHideDeleteKnowledgeModal Nothing
            }
    in
    modalView modalConfig


upgradeModal : Model -> Html Msgs.Msg
upgradeModal model =
    let
        ( visible, name ) =
            case model.kmToBeUpgraded of
                Just km ->
                    ( True, km.name )

                Nothing ->
                    ( False, "" )

        options =
            case model.packages of
                Success packages ->
                    ( "", "- select parent package -" ) :: List.map createOption packages

                _ ->
                    []

        modalContent =
            case model.packages of
                Unset ->
                    [ emptyNode ]

                Loading ->
                    [ fullPageLoader ]

                Error error ->
                    [ p [ class "alert alert-danger" ] [ text error ] ]

                Success packages ->
                    [ p [ class "alert alert-info" ]
                        [ text "Select the new parent package you want to migrate "
                        , strong [] [ text name ]
                        , text " to."
                        ]
                    , selectGroup options model.kmUpgradeForm "targetPackageId" "New parent package"
                        |> Html.map (UpgradeFormMsg >> Msgs.KnowledgeModelsIndexMsg)
                    ]

        modalConfig =
            { modalTitle = "Create migration"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.creatingMigration
            , actionName = "Create"
            , actionMsg = Msgs.KnowledgeModelsIndexMsg <| UpgradeFormMsg Form.Submit
            , cancelMsg = Msgs.KnowledgeModelsIndexMsg <| ShowHideUpgradeModal Nothing
            }
    in
    modalView modalConfig


createOption : PackageDetail -> ( String, String )
createOption package =
    let
        optionText =
            package.name ++ " " ++ package.version ++ " (" ++ package.id ++ ")"
    in
    ( package.id, optionText )
