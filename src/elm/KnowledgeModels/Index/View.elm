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
import KnowledgeModels.Common.Package exposing (Package)
import KnowledgeModels.Common.PackageState as PackageState
import KnowledgeModels.Common.Version as Version
import KnowledgeModels.Index.Models exposing (..)
import KnowledgeModels.Index.Msgs exposing (Msg(..))
import KnowledgeModels.Routing exposing (Route(..))
import Routing exposing (Route(..))


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView (viewKnowledgeModels appState model) model.packages


viewKnowledgeModels : AppState -> Model -> List Package -> Html Msg
viewKnowledgeModels appState model packages =
    div [ listClass "KnowledgeModels__Index" ]
        [ Page.header "Knowledge Models" (indexActions appState)
        , FormResult.successOnlyView model.deletingPackage
        , Listing.view (listingConfig appState) <| List.sortBy (String.toLower << .name) packages
        , deleteModal model
        ]


indexActions : AppState -> List (Html Msg)
indexActions appState =
    if hasPerm appState.jwt packageManagementWrite then
        [ linkTo (Routing.KnowledgeModels <| Import Nothing)
            [ class "btn btn-primary link-with-icon" ]
            [ i [ class "fa fa-upload" ] []
            , text "Import"
            ]
        ]

    else
        []


listingConfig : AppState -> ListingConfig Package Msg
listingConfig appState =
    { title = listingTitle
    , description = listingDescription
    , actions = listingActions appState
    , textTitle = .name
    , emptyText = "Click \"Import\" button to import a new Knowledge Model."
    , updated =
        Just
            { getTime = .createdAt
            , currentTime = appState.currentTime
            }
    }


listingTitle : Package -> Html Msg
listingTitle package =
    span []
        [ linkTo (detailRoute package) [] [ text package.name ]
        , span [ class "badge badge-light", title "Latest version" ] [ text <| Version.toString package.version ]
        , listingTitleOutdatedBadge package
        ]


listingTitleOutdatedBadge : Package -> Html Msg
listingTitleOutdatedBadge package =
    if PackageState.isOutdated package.state then
        span [ class "badge badge-warning" ] [ text "outdated" ]

    else
        emptyNode


listingDescription : Package -> Html Msg
listingDescription package =
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
                    span [ class "fragment", title "Published by" ]
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
                 , msg = ListingActionMsg <| ShowHideDeletePackage <| Just package
                 }
               ]

    else
        actions


detailRoute : Package -> Routing.Route
detailRoute package =
    Routing.KnowledgeModels <| Detail package.id


deleteModal : Model -> Html Msg
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
            , actionMsg = DeletePackage
            , cancelMsg = Just <| ShowHideDeletePackage Nothing
            , dangerous = True
            }
    in
    Modal.confirm modalConfig
