module KMPackages.Index.View exposing (view)

import Common.Html exposing (..)
import Common.View exposing (defaultFullPageError, fullPageActionResultView, fullPageLoader, modalView, pageHeader)
import Common.View.Forms exposing (formSuccessResultView)
import Common.View.Table exposing (TableAction(TableActionLink, TableActionMsg), TableActionLabel(TableActionIcon, TableActionText), TableConfig, TableFieldValue(TextValue), indexTable)
import Html exposing (..)
import Html.Attributes exposing (..)
import KMPackages.Index.Models exposing (..)
import KMPackages.Index.Msgs exposing (Msg(..))
import KMPackages.Models exposing (..)
import Msgs
import Routing exposing (Route(..))


view : Model -> Html Msgs.Msg
view model =
    div []
        [ pageHeader "Knowledge Model Packages" indexActions
        , formSuccessResultView model.deletingPackage
        , fullPageActionResultView (indexTable tableConfig Msgs.PackageManagementIndexMsg) model.packages
        , deleteModal model
        ]


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo KMPackagesImport
        [ class "btn btn-primary link-with-icon" ]
        [ i [ class "fa fa-cloud-upload" ] []
        , text "Import"
        ]
    ]


tableConfig : TableConfig Package Msg
tableConfig =
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
        ]
    , actions =
        [ { label = TableActionIcon "fa fa-trash-o"
          , action = TableActionMsg tableActionDelete
          , visible = always True
          }
        , { label = TableActionText "View detail"
          , action = TableActionLink tableActionViewDetail
          , visible = always True
          }
        ]
    }


tableActionDelete : (Msg -> Msgs.Msg) -> Package -> Msgs.Msg
tableActionDelete wrapMsg =
    wrapMsg << ShowHideDeletePackage << Just


tableActionViewDetail : Package -> Routing.Route
tableActionViewDetail package =
    Routing.KMPackagesDetail package.organizationId package.kmId


deleteModal : Model -> Html Msgs.Msg
deleteModal model =
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
            , actionMsg = Msgs.PackageManagementIndexMsg DeletePackage
            , cancelMsg = Msgs.PackageManagementIndexMsg <| ShowHideDeletePackage Nothing
            }
    in
    modalView modalConfig
