module KMPackages.Index.View exposing (view)

import Auth.Models exposing (JwtToken)
import Auth.Permission exposing (hasPerm, packageManagementWrite)
import Common.Html exposing (..)
import Common.View exposing (defaultFullPageError, fullPageActionResultView, fullPageLoader, modalView, pageHeader)
import Common.View.Forms exposing (formSuccessResultView)
import Common.View.Table exposing (TableAction(TableActionLink, TableActionMsg), TableActionLabel(TableActionIcon, TableActionText), TableConfig, TableFieldValue(TextValue), indexTable)
import Html exposing (..)
import Html.Attributes exposing (..)
import KMPackages.Common.Models exposing (..)
import KMPackages.Index.Models exposing (..)
import KMPackages.Index.Msgs exposing (Msg(..))
import KMPackages.Routing exposing (Route(..))
import Msgs
import Routing exposing (Route(..))


view : (Msg -> Msgs.Msg) -> Maybe JwtToken -> Model -> Html Msgs.Msg
view wrapMsg jwt model =
    div [ class "col KMPackages__Index" ]
        [ pageHeader "Knowledge Model Packages" (indexActions jwt)
        , formSuccessResultView model.deletingPackage
        , fullPageActionResultView (indexTable (tableConfig jwt) wrapMsg) model.packages
        , deleteModal wrapMsg model
        ]


indexActions : Maybe JwtToken -> List (Html Msgs.Msg)
indexActions jwt =
    if hasPerm jwt packageManagementWrite then
        [ linkTo (Routing.KMPackages Import)
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
        [ { label = TableActionIcon "fa fa-trash-o"
          , action = TableActionMsg tableActionDelete
          , visible = always <| hasPerm jwt packageManagementWrite
          }
        , { label = TableActionText "View detail"
          , action = TableActionLink tableActionViewDetail
          , visible = always True
          }
        ]
    , sortBy = .name
    }


tableActionDelete : (Msg -> Msgs.Msg) -> Package -> Msgs.Msg
tableActionDelete wrapMsg =
    wrapMsg << ShowHideDeletePackage << Just


tableActionViewDetail : Package -> Routing.Route
tableActionViewDetail package =
    Routing.KMPackages <| Detail package.organizationId package.kmId


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
    modalView modalConfig
