module Wizard.KMEditor.Index.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Locale exposing (l, lg, lh, lx)
import Version exposing (Version)
import Wizard.Auth.Permission as Perm exposing (hasPerm)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (..)
import Wizard.Common.Html.Attribute exposing (listClass)
import Wizard.Common.JwtToken exposing (JwtToken)
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Listing as Listing exposing (ListingActionConfig, ListingActionType(..), ListingConfig)
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.KMEditor.Common.Branch as Branch exposing (Branch)
import Wizard.KMEditor.Common.BranchState as BranchState
import Wizard.KMEditor.Common.BranchUtils as BranchUtils
import Wizard.KMEditor.Index.Models exposing (..)
import Wizard.KMEditor.Index.Msgs exposing (Msg(..))
import Wizard.KMEditor.Routes exposing (Route(..))
import Wizard.KnowledgeModels.Common.PackageDetail as PackageDetail exposing (PackageDetail)
import Wizard.KnowledgeModels.Routes
import Wizard.Routes as Routes
import Wizard.Utils exposing (listInsertIf, packageIdToComponents)


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
    Page.actionResultView appState (viewKMEditors appState model) model.branches


viewKMEditors : AppState -> Model -> List Branch -> Html Msg
viewKMEditors appState model branches =
    div [ listClass "KMEditor__Index" ]
        [ Page.header (l_ "header.title" appState) (indexActions appState)
        , FormResult.view appState model.deletingMigration
        , Listing.view appState (listingConfig appState) <| List.sortBy (String.toLower << .name) branches
        , deleteModal appState model
        , upgradeModal appState model
        ]


indexActions : AppState -> List (Html Msg)
indexActions appState =
    [ linkTo appState
        (Routes.KMEditorRoute <| CreateRoute Nothing)
        [ class "btn btn-primary" ]
        [ lx_ "header.create" appState ]
    ]


listingConfig : AppState -> ListingConfig Branch Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription appState
    , actions = listingActions appState
    , textTitle = .name
    , emptyText = l_ "content.empty" appState
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
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
            if continueMigrationActionVisible appState.jwt branch then
                linkTo appState (Routes.KMEditorRoute <| MigrationRoute <| branch.uuid)

            else
                span

        BranchState.Migrated ->
            if publishActionVisible appState.jwt branch then
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
    BranchUtils.lastVersion branch
        |> Maybe.map badge
        |> Maybe.withDefault emptyNode


listingTitleBadge : AppState -> Branch -> Html msg
listingTitleBadge appState branch =
    case branch.state of
        BranchState.Outdated ->
            span
                [ title <| l_ "badge.outdated.title" appState
                , class "badge badge-warning"
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


listingActions : AppState -> Branch -> List (ListingActionConfig Msg)
listingActions appState branch =
    let
        openEditor =
            { extraClass = Just "font-weight-bold"
            , icon = Nothing
            , label = l_ "action.openEditor" appState
            , msg = ListingActionLink (Routes.KMEditorRoute <| EditorRoute <| branch.uuid)
            }

        publish =
            { extraClass = Nothing
            , icon = Just <| faSet "kmEditorList.publish" appState
            , label = l_ "action.publish" appState
            , msg = ListingActionLink <| Routes.KMEditorRoute <| PublishRoute <| branch.uuid
            }

        upgrade =
            { extraClass = Nothing
            , icon = Just <| faSet "kmEditorList.upgrade" appState
            , label = l_ "action.upgrade" appState
            , msg = ListingActionMsg <| ShowHideUpgradeModal <| Just branch
            }

        continueMigration =
            { extraClass = Nothing
            , icon = Just <| faSet "kmEditorList.continueMigration" appState
            , label = l_ "action.continueMigration" appState
            , msg = ListingActionLink <| Routes.KMEditorRoute <| MigrationRoute <| branch.uuid
            }

        cancelMigration =
            { extraClass = Nothing
            , icon = Just <| faSet "_global.cancel" appState
            , label = l_ "action.cancelMigration" appState
            , msg = ListingActionMsg <| DeleteMigration <| branch.uuid
            }

        delete =
            { extraClass = Just "text-danger"
            , icon = Just <| faSet "_global.delete" appState
            , label = l_ "action.delete" appState
            , msg = ListingActionMsg <| ShowHideDeleteBranchModal <| Just branch
            }
    in
    []
        |> listInsertIf openEditor (openEditorActionVisible branch)
        |> listInsertIf publish (publishActionVisible appState.jwt branch)
        |> listInsertIf upgrade (upgradeActionVisible appState.jwt branch)
        |> listInsertIf continueMigration (continueMigrationActionVisible appState.jwt branch)
        |> listInsertIf cancelMigration (tableActionCancelMigrationVisible appState.jwt branch)
        |> listInsertIf delete True


openEditorActionVisible : Branch -> Bool
openEditorActionVisible =
    Branch.matchState [ BranchState.Default, BranchState.Edited, BranchState.Outdated ]


publishActionVisible : Maybe JwtToken -> Branch -> Bool
publishActionVisible jwt branch =
    hasPerm jwt Perm.knowledgeModelPublish && Branch.matchState [ BranchState.Edited, BranchState.Migrated ] branch


upgradeActionVisible : Maybe JwtToken -> Branch -> Bool
upgradeActionVisible jwt km =
    hasPerm jwt Perm.knowledgeModelUpgrade && Branch.matchState [ BranchState.Outdated ] km


continueMigrationActionVisible : Maybe JwtToken -> Branch -> Bool
continueMigrationActionVisible jwt km =
    hasPerm jwt Perm.knowledgeModelUpgrade && Branch.matchState [ BranchState.Migrating ] km


tableActionCancelMigrationVisible : Maybe JwtToken -> Branch -> Bool
tableActionCancelMigrationVisible jwt km =
    hasPerm jwt Perm.knowledgeModelUpgrade && Branch.matchState [ BranchState.Migrating, BranchState.Migrated ] km


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
