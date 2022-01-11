module Wizard.KMEditor.Index.View exposing (view)

import Html exposing (Attribute, Html, a, code, div, i, span, text)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onClick)
import Shared.Data.Branch exposing (Branch)
import Shared.Data.Branch.BranchState as BranchState
import Shared.Html exposing (emptyNode, faKeyClass, faSet)
import Shared.Locale exposing (l, lg, lx)
import Shared.Utils exposing (listInsertIf, packageIdToComponents)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ListingActionType(..), ListingDropdownItem, ViewConfig)
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, listClass)
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.KMEditor.Common.BranchUtils as BranchUtils
import Wizard.KMEditor.Common.DeleteModal as DeleteModal
import Wizard.KMEditor.Common.UpgradeModal as UpgradeModal
import Wizard.KMEditor.Editor.KMEditorRoute as KMEditorRoute
import Wizard.KMEditor.Index.Models exposing (Model)
import Wizard.KMEditor.Index.Msgs exposing (Msg(..))
import Wizard.KMEditor.Routes exposing (Route(..))
import Wizard.KnowledgeModels.Routes
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Index.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KMEditor.Index.View"


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "KMEditor__Index" ]
        [ Page.header (l_ "header.title" appState) []
        , FormResult.view appState model.deletingMigration
        , Listing.view appState (listingConfig appState) model.branches
        , Html.map DeleteModalMsg <| DeleteModal.view appState model.deleteModal
        , Html.map UpgradeModalMsg <| UpgradeModal.view appState model.upgradeModal
        ]


createButton : AppState -> Html Msg
createButton appState =
    linkTo appState
        (Routes.KMEditorRoute <| CreateRoute Nothing Nothing)
        [ class "btn btn-primary"
        , dataCy "km-editor_create-button"
        ]
        [ lx_ "header.create" appState ]


listingConfig : AppState -> ViewConfig Branch Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription appState
    , itemAdditionalData = always Nothing
    , dropdownItems = listingActions appState
    , textTitle = .name
    , emptyText = l_ "listing.empty" appState
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , searchPlaceholderText = Just (l_ "listing.searchPlaceholderText" appState)
    , sortOptions =
        [ ( "name", lg "branch.name" appState )
        , ( "createdAt", lg "branch.createdAt" appState )
        , ( "updatedAt", lg "branch.updatedAt" appState )
        ]
    , filters = []
    , toRoute = \_ -> Routes.KMEditorRoute << IndexRoute
    , toolbarExtra = Just (createButton appState)
    }


listingTitle : AppState -> Branch -> Html Msg
listingTitle appState branch =
    span []
        [ linkToKM appState branch [] [ text branch.name ]
        , listingTitleLastPublishedVersionBadge appState branch
        , listingTitleBadge appState branch
        ]


linkToKM : AppState -> Branch -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
linkToKM appState branch =
    case branch.state of
        BranchState.Migrating ->
            if Feature.knowledgeModelEditorContinueMigration appState branch then
                linkTo appState (Routes.KMEditorRoute <| MigrationRoute branch.uuid)

            else
                span

        BranchState.Migrated ->
            if Feature.knowledgeModelEditorPublish appState branch then
                linkTo appState (Routes.KMEditorRoute <| PublishRoute branch.uuid)

            else
                span

        _ ->
            linkTo appState (Routes.KMEditorRoute <| EditorRoute branch.uuid (KMEditorRoute.Edit Nothing))


listingTitleLastPublishedVersionBadge : AppState -> Branch -> Html msg
listingTitleLastPublishedVersionBadge appState branch =
    let
        badge version =
            span [ title <| l_ "badge.lastPublishedVersion.title" appState, class "badge badge-light" ]
                [ text <| Version.toString version ]
    in
    BranchUtils.lastVersion appState branch
        |> Maybe.map badge
        |> Maybe.withDefault emptyNode


listingTitleBadge : AppState -> Branch -> Html Msg
listingTitleBadge appState branch =
    case branch.state of
        BranchState.Outdated ->
            a
                [ title <| l_ "badge.outdated.title" appState
                , class "badge badge-warning"
                , onClick (UpgradeModalMsg (UpgradeModal.open branch.uuid branch.name (Maybe.withDefault "" branch.forkOfPackageId)))
                , dataCy "km-editor_list_outdated-badge"
                ]
                [ lx_ "badge.outdated" appState ]

        BranchState.Migrating ->
            span
                [ title <| l_ "badge.migrating.title" appState
                , class "badge badge-info"
                ]
                [ lx_ "badge.migrating" appState ]

        BranchState.Migrated ->
            span
                [ title <| l_ "badge.migrated.title" appState
                , class "badge badge-success"
                ]
                [ lx_ "badge.migrated" appState ]

        BranchState.Edited ->
            i
                [ title <| l_ "badge.edited.title" appState
                , class <| faKeyClass "kmEditorList.edited" appState
                ]
                []

        _ ->
            emptyNode


listingDescription : AppState -> Branch -> Html Msg
listingDescription appState branch =
    let
        parent =
            case branch.forkOfPackageId of
                Just forkOfPackageId ->
                    let
                        elem =
                            case packageIdToComponents forkOfPackageId of
                                Just ( orgId, kmId, version ) ->
                                    linkTo appState (Routes.KnowledgeModelsRoute <| Wizard.KnowledgeModels.Routes.DetailRoute <| orgId ++ ":" ++ kmId ++ ":" ++ version)

                                _ ->
                                    span
                    in
                    elem [ class "fragment", title <| lg "package.parentKM" appState ]
                        [ faSet "km.fork" appState
                        , text forkOfPackageId
                        ]

                Nothing ->
                    emptyNode
    in
    span []
        [ span [ class "fragment" ] [ code [] [ text branch.kmId ] ]
        , parent
        ]


listingActions : AppState -> Branch -> List (ListingDropdownItem Msg)
listingActions appState branch =
    let
        openEditor =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmEditorList.edit" appState
                , label = l_ "action.openEditor" appState
                , msg = ListingActionLink (Routes.KMEditorRoute <| EditorRoute branch.uuid (KMEditorRoute.Edit Nothing))
                , dataCy = "open-editor"
                }

        publish =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmEditorList.publish" appState
                , label = l_ "action.publish" appState
                , msg = ListingActionLink <| Routes.KMEditorRoute <| PublishRoute branch.uuid
                , dataCy = "publish"
                }

        upgrade =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmEditorList.upgrade" appState
                , label = l_ "action.upgrade" appState
                , msg = ListingActionMsg <| UpgradeModalMsg (UpgradeModal.open branch.uuid branch.name (Maybe.withDefault "" branch.forkOfPackageId))
                , dataCy = "upgrade"
                }

        continueMigration =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmEditorList.continueMigration" appState
                , label = l_ "action.continueMigration" appState
                , msg = ListingActionLink <| Routes.KMEditorRoute <| MigrationRoute branch.uuid
                , dataCy = "continue-migration"
                }

        cancelMigration =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.cancel" appState
                , label = l_ "action.cancelMigration" appState
                , msg = ListingActionMsg <| DeleteMigration branch.uuid
                , dataCy = "cancel-migration"
                }

        delete =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
                , label = l_ "action.delete" appState
                , msg = ListingActionMsg <| DeleteModalMsg (DeleteModal.open branch.uuid branch.name)
                , dataCy = "delete-migration"
                }

        showOpenEditor =
            Feature.knowledgeModelEditorOpen appState branch

        showPublish =
            Feature.knowledgeModelEditorPublish appState branch

        showUpgrade =
            Feature.knowledgeModelEditorUpgrade appState branch

        showContinueMigration =
            Feature.knowledgeModelEditorContinueMigration appState branch

        showCancelMigration =
            Feature.knowledgeModelEditorCancelMigration appState branch

        showDelete =
            Feature.knowledgeModelEditorDelete appState branch
    in
    []
        |> listInsertIf openEditor showOpenEditor
        |> listInsertIf Listing.dropdownSeparator showPublish
        |> listInsertIf publish showPublish
        |> listInsertIf Listing.dropdownSeparator (showUpgrade || showContinueMigration || showCancelMigration)
        |> listInsertIf upgrade showUpgrade
        |> listInsertIf continueMigration showContinueMigration
        |> listInsertIf cancelMigration showCancelMigration
        |> listInsertIf Listing.dropdownSeparator showDelete
        |> listInsertIf delete showDelete
