module Wizard.KMEditor.Index.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Shared.Auth.Permission as Perm
import Shared.Auth.Session exposing (Session)
import Shared.Data.Branch as Branch exposing (Branch)
import Shared.Data.Branch.BranchState as BranchState
import Shared.Data.PackageDetail as PackageDetail exposing (PackageDetail)
import Shared.Html exposing (emptyNode, faKeyClass, faSet)
import Shared.Locale exposing (l, lg, lh, lx)
import Shared.Utils exposing (listInsertIf, packageIdToComponents)
import Version exposing (Version)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ListingActionConfig, ListingActionType(..), ListingDropdownItem, ViewConfig)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.KMEditor.Common.BranchUtils as BranchUtils
import Wizard.KMEditor.Index.Models exposing (..)
import Wizard.KMEditor.Index.Msgs exposing (Msg(..))
import Wizard.KMEditor.Routes exposing (Route(..))
import Wizard.KnowledgeModels.Routes
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Index.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.KMEditor.Index.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KMEditor.Index.View"


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "KMEditor__Index" ]
        [ Page.header (l_ "header.title" appState) []
        , FormResult.view appState model.deletingMigration
        , Listing.view appState (listingConfig appState) model.branches
        , deleteModal appState model
        , upgradeModal appState model
        ]


createButton : AppState -> Html Msg
createButton appState =
    linkTo appState
        (Routes.KMEditorRoute <| CreateRoute Nothing Nothing)
        [ class "btn btn-primary" ]
        [ lx_ "header.create" appState ]


listingConfig : AppState -> ViewConfig Branch Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription appState
    , dropdownItems = listingActions appState
    , textTitle = .name
    , emptyText = l_ "content.empty" appState
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , sortOptions =
        [ ( "name", lg "branch.name" appState )
        , ( "createdAt", lg "branch.createdAt" appState )
        , ( "updatedAt", lg "branch.updatedAt" appState )
        ]
    , toRoute = Routes.KMEditorRoute << IndexRoute
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
            if continueMigrationActionVisible appState.session branch then
                linkTo appState (Routes.KMEditorRoute <| MigrationRoute <| branch.uuid)

            else
                span

        BranchState.Migrated ->
            if publishActionVisible appState.session branch then
                linkTo appState (Routes.KMEditorRoute <| PublishRoute <| branch.uuid)

            else
                span

        _ ->
            linkTo appState (Routes.KMEditorRoute <| EditorRoute <| branch.uuid)


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
                , onClick (ShowHideUpgradeModal <| Just branch)
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
                , msg = ListingActionLink (Routes.KMEditorRoute <| EditorRoute branch.uuid)
                }

        publish =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmEditorList.publish" appState
                , label = l_ "action.publish" appState
                , msg = ListingActionLink <| Routes.KMEditorRoute <| PublishRoute branch.uuid
                }

        upgrade =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmEditorList.upgrade" appState
                , label = l_ "action.upgrade" appState
                , msg = ListingActionMsg <| ShowHideUpgradeModal <| Just branch
                }

        continueMigration =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmEditorList.continueMigration" appState
                , label = l_ "action.continueMigration" appState
                , msg = ListingActionLink <| Routes.KMEditorRoute <| MigrationRoute branch.uuid
                }

        cancelMigration =
            Listing.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.cancel" appState
                , label = l_ "action.cancelMigration" appState
                , msg = ListingActionMsg <| DeleteMigration branch.uuid
                }

        delete =
            Listing.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
                , label = l_ "action.delete" appState
                , msg = ListingActionMsg <| ShowHideDeleteBranchModal <| Just branch
                }

        showOpenEditor =
            openEditorActionVisible branch

        showPublish =
            publishActionVisible appState.session branch

        showUpgrade =
            upgradeActionVisible appState.session branch

        showContinueMigration =
            continueMigrationActionVisible appState.session branch

        showCancelMigration =
            tableActionCancelMigrationVisible appState.session branch
    in
    []
        |> listInsertIf openEditor showOpenEditor
        |> listInsertIf Listing.dropdownSeparator showPublish
        |> listInsertIf publish showPublish
        |> listInsertIf Listing.dropdownSeparator (showUpgrade || showContinueMigration || showCancelMigration)
        |> listInsertIf upgrade showUpgrade
        |> listInsertIf continueMigration showContinueMigration
        |> listInsertIf cancelMigration showCancelMigration
        |> listInsertIf Listing.dropdownSeparator True
        |> listInsertIf delete True


openEditorActionVisible : Branch -> Bool
openEditorActionVisible =
    Branch.matchState [ BranchState.Default, BranchState.Edited, BranchState.Outdated ]


publishActionVisible : Session -> Branch -> Bool
publishActionVisible session branch =
    Perm.hasPerm session Perm.knowledgeModelPublish && Branch.matchState [ BranchState.Edited, BranchState.Migrated ] branch


upgradeActionVisible : Session -> Branch -> Bool
upgradeActionVisible session km =
    Perm.hasPerm session Perm.knowledgeModelUpgrade && Branch.matchState [ BranchState.Outdated ] km


continueMigrationActionVisible : Session -> Branch -> Bool
continueMigrationActionVisible session km =
    Perm.hasPerm session Perm.knowledgeModelUpgrade && Branch.matchState [ BranchState.Migrating ] km


tableActionCancelMigrationVisible : Session -> Branch -> Bool
tableActionCancelMigrationVisible session km =
    Perm.hasPerm session Perm.knowledgeModelUpgrade && Branch.matchState [ BranchState.Migrating, BranchState.Migrated ] km


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    let
        ( visible, name ) =
            case model.branchToBeDeleted of
                Just branch ->
                    ( True, branch.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                (lh_ "deleteModal.text" [ strong [] [ text name ] ] appState)
            ]

        modalConfig =
            { modalTitle = l_ "deleteModal.title" appState
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingKnowledgeModel
            , actionName = l_ "deleteModal.action" appState
            , actionMsg = DeleteBranch
            , cancelMsg = Just <| ShowHideDeleteBranchModal Nothing
            , dangerous = True
            }
    in
    Modal.confirm appState modalConfig


upgradeModal : AppState -> Model -> Html Msg
upgradeModal appState model =
    let
        ( visible, name ) =
            case model.branchToBeUpgraded of
                Just branch ->
                    ( True, branch.name )

                Nothing ->
                    ( False, "" )

        options =
            case model.package of
                Success package ->
                    ( "", l_ "upgradeModal.form.defaultOption" appState ) :: PackageDetail.createFormOptions package

                _ ->
                    []

        modalContent =
            case model.package of
                Unset ->
                    [ emptyNode ]

                Loading ->
                    [ Page.loader appState ]

                Error error ->
                    [ p [ class "alert alert-danger" ] [ text error ] ]

                Success _ ->
                    [ p [ class "alert alert-info" ]
                        (lh_ "upgradeModal.text" [ strong [] [ text name ] ] appState)
                    , FormGroup.select appState options model.branchUpgradeForm "targetPackageId" (l_ "upgradeModal.form.targetPackageId" appState)
                        |> Html.map UpgradeFormMsg
                    ]

        modalConfig =
            { modalTitle = l_ "upgradeModal.title" appState
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.creatingMigration
            , actionName = l_ "upgradeModal.action" appState
            , actionMsg = UpgradeFormMsg Form.Submit
            , cancelMsg = Just <| ShowHideUpgradeModal Nothing
            , dangerous = False
            }
    in
    Modal.confirm appState modalConfig
