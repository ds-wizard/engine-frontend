module KnowledgeModels.Index.View exposing (view)

import Auth.Permission exposing (hasPerm, packageManagementWrite)
import Common.AppState exposing (AppState)
import Common.Html exposing (..)
import Common.Html.Attribute exposing (listClass)
import Common.View.FormResult as FormResult
import Common.View.Listing as Listing exposing (ListingActionConfig, ListingActionType(..), ListingConfig)
import Common.View.Modal as Modal
import Common.View.Page as Page
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
    div [ listClass "KnowledgeModels__Index" ]
        [ Page.header "Knowledge Models" (indexActions appState)
        , FormResult.successOnlyView model.deletingPackage
        , Listing.view (listingConfig wrapMsg appState) <| List.sortBy .name packages
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


listingConfig : (Msg -> Msgs.Msg) -> AppState -> ListingConfig Package Msgs.Msg
listingConfig wrapMsg appState =
    { title = listingTitle
    , description = listingDescription
    , actions = listingActions wrapMsg appState
    , textTitle = .name
    , emptyText = "Click \"Import\" button to import a new Knowledge Model."
    }


listingTitle : Package -> Html Msgs.Msg
listingTitle package =
    span []
        [ linkTo (detailRoute package) [] [ text package.name ]
        ]


listingDescription : Package -> Html Msgs.Msg
listingDescription package =
    span []
        [ span [ class "fragment", title "Organization ID" ] [ text package.organizationId ]
        , span [ class "fragment", title "Knowledge Model ID" ] [ text package.kmId ]
        , span [ class "fragment", title "Latest Version" ] [ text package.latestVersion ]
        ]


listingActions : (Msg -> Msgs.Msg) -> AppState -> Package -> List (ListingActionConfig Msgs.Msg)
listingActions wrapMsg appState package =
    let
        actions =
            [ { extraClass = Nothing
              , icon = Just "eye"
              , label = "View detail"
              , msg = ListingActionLink (detailRoute package)
              }
            ]
    in
    if hasPerm appState.jwt packageManagementWrite then
        actions
            ++ [ { extraClass = Just "text-danger"
                 , icon = Just "trash-o"
                 , label = "Delete"
                 , msg = ListingActionMsg (wrapMsg <| ShowHideDeletePackage <| Just package)
                 }
               ]

    else
        actions


detailRoute : Package -> Routing.Route
detailRoute package =
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
