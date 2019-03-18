module KnowledgeModels.Index.View exposing (view)

import Auth.Models exposing (JwtToken)
import Auth.Permission exposing (hasPerm, packageManagementWrite)
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


view : (Msg -> Msgs.Msg) -> Maybe JwtToken -> Model -> Html Msgs.Msg
view wrapMsg jwt model =
    div [ class "col KnowledgeModels__Index" ]
        [ Page.header "Knowledge Models" (indexActions jwt)
        , FormResult.successOnlyView model.deletingPackage
        , Page.actionResultView (Table.view (tableConfig jwt) wrapMsg) model.packages
        , deleteModal wrapMsg model
        ]


indexActions : Maybe JwtToken -> List (Html Msgs.Msg)
indexActions jwt =
    if hasPerm jwt packageManagementWrite then
        [ linkTo (Routing.KnowledgeModels Import)
            [ class "btn btn-primary link-with-icon" ]
            [ i [ class "fa fa-cloud-upload" ] []
            , text "Import"
            ]
        ]

    else
        []


tableConfig : Maybe JwtToken -> TableConfig Package Msg
tableConfig jwt =
    { emptyMessage = "There are no packages."
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
          , visible = always <| hasPerm jwt packageManagementWrite
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
