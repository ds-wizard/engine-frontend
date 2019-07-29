module KMEditor.Index.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import Common.AppState exposing (AppState)
import Common.Html exposing (..)
import Common.Html.Attribute exposing (listClass)
import Common.View.FormGroup as FormGroup
import Common.View.FormResult as FormResult
import Common.View.Listing as Listing exposing (ListingActionConfig, ListingActionType(..), ListingConfig)
import Common.View.Modal as Modal
import Common.View.Page as Page
import Form
import Html exposing (..)
import Html.Attributes exposing (..)
import KMEditor.Common.Branch as Branch exposing (Branch)
import KMEditor.Common.BranchState as BranchState
import KMEditor.Common.BranchUtils as BranchUtils
import KMEditor.Index.Models exposing (..)
import KMEditor.Index.Msgs exposing (Msg(..))
import KMEditor.Routing exposing (Route(..))
import KnowledgeModels.Common.PackageDetail as PackageDetail exposing (PackageDetail)
import KnowledgeModels.Common.Version as Version exposing (Version)
import KnowledgeModels.Routing
import Msgs
import Routing exposing (Route(..))
import Utils exposing (listInsertIf, packageIdToComponents)


view : (Msg -> Msgs.Msg) -> AppState -> Model -> Html Msgs.Msg
view wrapMsg appState model =
    Page.actionResultView (viewKMEditors wrapMsg appState model) model.branches


viewKMEditors : (Msg -> Msgs.Msg) -> AppState -> Model -> List Branch -> Html Msgs.Msg
viewKMEditors wrapMsg appState model branches =
    div [ listClass "KMEditor__Index" ]
        [ Page.header "Knowledge Model Editor" indexActions
        , FormResult.view model.deletingMigration
        , Listing.view (listingConfig wrapMsg appState) <| List.sortBy (String.toLower << .name) branches
        , deleteModal wrapMsg model
        , upgradeModal wrapMsg model
        ]


indexActions : List (Html Msgs.Msg)
indexActions =
    [ linkTo (Routing.KMEditor <| CreateRoute Nothing)
        [ class "btn btn-primary" ]
        [ text "Create" ]
    ]


listingConfig : (Msg -> Msgs.Msg) -> AppState -> ListingConfig Branch Msgs.Msg
listingConfig wrapMsg appState =
    { title = listingTitle appState.jwt
    , description = listingDescription
    , actions = listingActions wrapMsg appState.jwt
    , textTitle = .name
    , emptyText = "Click \"Create\" button to add a new Knowledge Model Editor."
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    }


listingTitle : Maybe JwtToken -> Branch -> Html Msgs.Msg
listingTitle mbJwt km =
    span []
        [ linkToKM mbJwt km [] [ text km.name ]
        , listingTitleLastPublishedVersionBadge km
        , listingTitleBadge km
        ]


linkToKM : Maybe JwtToken -> Branch -> List (Attribute Msgs.Msg) -> List (Html Msgs.Msg) -> Html Msgs.Msg
linkToKM mbJwt branch =
    case branch.state of
        BranchState.Migrating ->
            if continueMigrationActionVisible mbJwt branch then
                linkTo (Routing.KMEditor <| MigrationRoute <| branch.uuid)

            else
                span

        BranchState.Migrated ->
            if publishActionVisible mbJwt branch then
                linkTo (Routing.KMEditor <| PublishRoute <| branch.uuid)

            else
                span

        _ ->
            linkTo (Routing.KMEditor <| EditorRoute <| branch.uuid)


listingTitleLastPublishedVersionBadge : Branch -> Html msg
listingTitleLastPublishedVersionBadge branch =
    let
        badge version =
            span [ title "Last published version", class "badge badge-light" ]
                [ text <| Version.toString version ]
    in
    BranchUtils.lastVersion branch
        |> Maybe.map badge
        |> Maybe.withDefault emptyNode


listingTitleBadge : Branch -> Html msg
listingTitleBadge branch =
    case branch.state of
        BranchState.Outdated ->
            span
                [ title "There is a new version of parent knowledge model"
                , class "badge badge-warning"
                ]
                [ text "outdated" ]

        BranchState.Migrating ->
            span
                [ title "This editor is in the process of migration to a new parent knowledge model"
                , class "badge badge-info"
                ]
                [ text "migrating" ]

        BranchState.Migrated ->
            span
                [ title "This editor has been migrated to a new parent knowledge model, you can publish it now."
                , class "badge badge-success"
                ]
                [ text "migrated" ]

        BranchState.Edited ->
            i
                [ title "This editor contains unpublished changes"
                , class <| "fa fa-pencil"
                ]
                []

        _ ->
            emptyNode


listingDescription : Branch -> Html Msgs.Msg
listingDescription branch =
    let
        parent =
            case branch.forkOfPackageId of
                Just forkOfPackageId ->
                    let
                        elem =
                            case packageIdToComponents forkOfPackageId of
                                Just ( orgId, kmId, version ) ->
                                    linkTo (Routing.KnowledgeModels <| KnowledgeModels.Routing.Detail <| orgId ++ ":" ++ kmId ++ ":" ++ version)

                                _ ->
                                    span
                    in
                    elem [ class "fragment", title "Parent Knowledge Model" ]
                        [ fa "code-fork"
                        , text forkOfPackageId
                        ]

                Nothing ->
                    emptyNode
    in
    span []
        [ span [ class "fragment" ] [ code [] [ text branch.kmId ] ]
        , parent
        ]


listingActions : (Msg -> Msgs.Msg) -> Maybe JwtToken -> Branch -> List (ListingActionConfig Msgs.Msg)
listingActions wrapMsg mbJwt branch =
    let
        openEditor =
            { extraClass = Just "font-weight-bold"
            , icon = Nothing
            , label = "Open Editor"
            , msg = ListingActionLink (Routing.KMEditor <| EditorRoute <| branch.uuid)
            }

        publish =
            { extraClass = Nothing
            , icon = Just "cloud-upload"
            , label = "Publish"
            , msg = ListingActionLink <| Routing.KMEditor <| PublishRoute <| branch.uuid
            }

        upgrade =
            { extraClass = Nothing
            , icon = Just "angle-double-up"
            , label = "Upgrade"
            , msg = ListingActionMsg <| wrapMsg <| ShowHideUpgradeModal <| Just branch
            }

        continueMigration =
            { extraClass = Nothing
            , icon = Just "long-arrow-right"
            , label = "Continue Migration"
            , msg = ListingActionLink <| Routing.KMEditor <| MigrationRoute <| branch.uuid
            }

        cancelMigration =
            { extraClass = Nothing
            , icon = Just "ban"
            , label = "Cancel Migration"
            , msg = ListingActionMsg <| wrapMsg <| DeleteMigration <| branch.uuid
            }

        delete =
            { extraClass = Just "text-danger"
            , icon = Just "trash-o"
            , label = "Delete"
            , msg = ListingActionMsg <| wrapMsg <| ShowHideDeleteBranchModal <| Just branch
            }
    in
    []
        |> listInsertIf openEditor (openEditorActionVisible branch)
        |> listInsertIf publish (publishActionVisible mbJwt branch)
        |> listInsertIf upgrade (upgradeActionVisible mbJwt branch)
        |> listInsertIf continueMigration (continueMigrationActionVisible mbJwt branch)
        |> listInsertIf cancelMigration (tableActionCancelMigrationVisible mbJwt branch)
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


deleteModal : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
deleteModal wrapMsg model =
    let
        ( visible, name ) =
            case model.branchToBeDeleted of
                Just branch ->
                    ( True, branch.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                [ text "Are you sure you want to permanently delete "
                , strong [] [ text name ]
                , text "?"
                ]
            ]

        modalConfig =
            { modalTitle = "Delete knowledge model"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.deletingKnowledgeModel
            , actionName = "Delete"
            , actionMsg = wrapMsg DeleteBranch
            , cancelMsg = Just <| wrapMsg <| ShowHideDeleteBranchModal Nothing
            , dangerous = True
            }
    in
    Modal.confirm modalConfig


upgradeModal : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
upgradeModal wrapMsg model =
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
                    ( "", "- select parent package -" ) :: PackageDetail.createFormOptions package

                _ ->
                    []

        modalContent =
            case model.package of
                Unset ->
                    [ emptyNode ]

                Loading ->
                    [ Page.loader ]

                Error error ->
                    [ p [ class "alert alert-danger" ] [ text error ] ]

                Success _ ->
                    [ p [ class "alert alert-info" ]
                        [ text "Select the new parent package you want to migrate "
                        , strong [] [ text name ]
                        , text " to."
                        ]
                    , FormGroup.select options model.branchUpgradeForm "targetPackageId" "New parent package"
                        |> Html.map (wrapMsg << UpgradeFormMsg)
                    ]

        modalConfig =
            { modalTitle = "Create migration"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.creatingMigration
            , actionName = "Create"
            , actionMsg = wrapMsg <| UpgradeFormMsg Form.Submit
            , cancelMsg = Just <| wrapMsg <| ShowHideUpgradeModal Nothing
            , dangerous = False
            }
    in
    Modal.confirm modalConfig
