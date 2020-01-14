module Wizard.KnowledgeModels.Index.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Locale exposing (l, lg, lh, lx)
import Version
import Wizard.Auth.Permission exposing (hasPerm, packageManagementWrite)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (..)
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Listing as Listing exposing (ListingActionConfig, ListingActionType(..), ListingConfig)
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.KnowledgeModels.Common.Package exposing (Package)
import Wizard.KnowledgeModels.Common.PackageState as PackageState
import Wizard.KnowledgeModels.Index.Models exposing (..)
import Wizard.KnowledgeModels.Index.Msgs exposing (Msg(..))
import Wizard.KnowledgeModels.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.KnowledgeModels.Index.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.KnowledgeModels.Index.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KnowledgeModels.Index.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewKnowledgeModels appState model) model.packages


viewKnowledgeModels : AppState -> Model -> List Package -> Html Msg
viewKnowledgeModels appState model packages =
    div [ listClass "KnowledgeModels__Index" ]
        [ Page.header (l_ "header.title" appState) (indexActions appState)
        , FormResult.successOnlyView appState model.deletingPackage
        , Listing.view appState (listingConfig appState) <| List.sortBy (String.toLower << .name) packages
        , deleteModal appState model
        ]


indexActions : AppState -> List (Html Msg)
indexActions appState =
    if hasPerm appState.jwt packageManagementWrite then
        [ linkTo appState
            (Routes.KnowledgeModelsRoute <| ImportRoute Nothing)
            [ class "btn btn-primary link-with-icon" ]
            [ faSet "kms.upload" appState
            , lx_ "header.import" appState
            ]
        ]

    else
        []


listingConfig : AppState -> ListingConfig Package Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription appState
    , actions = listingActions appState
    , textTitle = .name
    , emptyText = l_ "listing.empty" appState
    , updated =
        Just
            { getTime = .createdAt
            , currentTime = appState.currentTime
            }
    }


listingTitle : AppState -> Package -> Html Msg
listingTitle appState package =
    span []
        [ linkTo appState (detailRoute package) [] [ text package.name ]
        , span
            [ class "badge badge-light"
            , title <| lg "package.latestVersion" appState
            ]
            [ text <| Version.toString package.version ]
        , listingTitleOutdatedBadge appState package
        ]


listingTitleOutdatedBadge : AppState -> Package -> Html Msg
listingTitleOutdatedBadge appState package =
    if PackageState.isOutdated package.state then
        span [ class "badge badge-warning" ] [ lx_ "badge.outdated" appState ]

    else
        emptyNode


listingDescription : AppState -> Package -> Html Msg
listingDescription appState package =
    let
        organizationFragment =
            case package.organization of
                Just organization ->
                    let
                        logo =
                            case organization.logo of
                                Just organizationLogo ->
                                    img [ class "organization-image", src organizationLogo ] []

                                Nothing ->
                                    emptyNode
                    in
                    span [ class "fragment", title <| lg "package.publishedBy" appState ]
                        [ logo
                        , text organization.name
                        ]

                Nothing ->
                    emptyNode
    in
    span []
        [ code [ class "fragment" ] [ text package.id ]
        , organizationFragment
        , span [ class "fragment" ] [ text package.description ]
        ]


listingActions : AppState -> Package -> List (ListingActionConfig Msg)
listingActions appState package =
    let
        actions =
            [ { extraClass = Just "font-weight-bold"
              , icon = Nothing
              , label = l_ "action.viewDetail" appState
              , msg = ListingActionLink (detailRoute package)
              }
            ]
    in
    if hasPerm appState.jwt packageManagementWrite then
        actions
            ++ [ { extraClass = Just "text-danger"
                 , icon = Just <| faSet "_global.delete" appState
                 , label = l_ "action.delete" appState
                 , msg = ListingActionMsg <| ShowHideDeletePackage <| Just package
                 }
               ]

    else
        actions


detailRoute : Package -> Routes.Route
detailRoute package =
    Routes.KnowledgeModelsRoute <| DetailRoute package.id


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    let
        ( visible, version ) =
            case model.packageToBeDeleted of
                Just package ->
                    ( True, package.organizationId ++ ":" ++ package.kmId )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                (lh_ "deleteModal.message" [ strong [] [ text version ] ] appState)
            ]

        modalConfig =
            { modalTitle = l_ "deleteModal.title" appState
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingPackage
            , actionName = l_ "deleteModal.action" appState
            , actionMsg = DeletePackage
            , cancelMsg = Just <| ShowHideDeletePackage Nothing
            , dangerous = True
            }
    in
    Modal.confirm appState modalConfig
