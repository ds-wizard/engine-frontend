module KnowledgeModels.Index.View exposing (view)

import Auth.Permission exposing (hasPerm, packageManagementWrite)
import Common.AppState exposing (AppState)
import Common.Html exposing (..)
import Common.View.FormResult as FormResult
import Common.View.Modal as Modal
import Common.View.Page as Page
import Common.View.Table as Table exposing (TableAction(..), TableActionLabel(..), TableConfig, TableFieldValue(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import KnowledgeModels.Common.Models exposing (..)
import KnowledgeModels.Index.Models exposing (..)
import KnowledgeModels.Index.Msgs exposing (Msg(..))
import KnowledgeModels.Routing exposing (Route(..))
import Msgs
import Routing exposing (Route(..))


view : (Msg -> Msgs.Msg) -> AppState -> Model -> Html Msgs.Msg
view wrapMsg appState model =
    Page.actionResultView (viewKnowledgeModels wrapMsg appState model) model.packages


viewKnowledgeModels : (Msg -> Msgs.Msg) -> AppState -> Model -> List Package -> Html Msgs.Msg
viewKnowledgeModels wrapMsg appState model packages =
    div [ class "col KnowledgeModels__Index" ]
        [ Page.header "Knowledge Models" (indexActions appState)
        , FormResult.successOnlyView model.deletingPackage
        , Table.view (tableConfig appState) wrapMsg packages
        , deleteModal wrapMsg model
        ]


indexActions : AppState -> List (Html Msgs.Msg)
indexActions appState =
    if hasPerm appState.jwt packageManagementWrite then
        [ linkTo (Routing.KnowledgeModels Import)
            [ class "btn btn-primary link-with-icon" ]
            [ i [ class "fa fa-cloud-upload" ] []
            , text "Import"
            ]
        ]

    else
        []


tableConfig : AppState -> TableConfig Package Msg
tableConfig appState =
    { emptyMessage = "There are no knowledge models."
    , fields =
        [ { label = "Name"
          , getValue = TextValue .name
          }
        , { label = "Organization ID"
          , getValue = TextValue .organizationId
          }
        , { label = "Knowledge Model ID"
          , getValue = TextValue .kmId
          }
        , { label = "Latest Version"
          , getValue = TextValue .latestVersion
          }
        ]
    , actions =
        [ { label = TableActionDefault "eye" "View detail"
          , action = TableActionLink tableActionViewDetail
          , visible = always True
          }
        , { label = TableActionDestructive "trash-o" "Delete"
          , action = TableActionMsg tableActionDelete
          , visible = always <| hasPerm appState.jwt packageManagementWrite
          }
        ]
    , sortBy = .name
    }


tableActionDelete : (Msg -> Msgs.Msg) -> Package -> Msgs.Msg
tableActionDelete wrapMsg =
    wrapMsg << ShowHideDeletePackage << Just


tableActionViewDetail : Package -> Routing.Route
tableActionViewDetail package =
    Routing.KnowledgeModels <| Detail package.organizationId package.kmId


deleteModal : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
deleteModal wrapMsg model =
    let
        ( visible, version ) =
            case model.packageToBeDeleted of
                Just package ->
                    ( True, package.organizationId ++ ":" ++ package.kmId )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                [ text "Are you sure you want to permanently delete "
                , strong [] [ text version ]
                , text " and all its versions?"
                ]
            ]

        modalConfig =
            { modalTitle = "Delete package"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingPackage
            , actionName = "Delete"
            , actionMsg = wrapMsg DeletePackage
            , cancelMsg = Just <| wrapMsg <| ShowHideDeletePackage Nothing
            }
    in
    Modal.confirm modalConfig
