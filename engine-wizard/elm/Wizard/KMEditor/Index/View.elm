module Wizard.KMEditor.Index.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Attribute, Html, a, code, div, span, text)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onClick)
import Shared.Components.Badge as Badge
import Shared.Html exposing (emptyNode, faSet)
import Shared.Utils exposing (packageIdToComponents)
import Version
import Wizard.Api.Models.Branch exposing (Branch)
import Wizard.Api.Models.Branch.BranchState as BranchState
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Common.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, listClass, tooltip)
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.KMEditor.Common.BranchUtils as BranchUtils
import Wizard.KMEditor.Common.DeleteModal as DeleteModal
import Wizard.KMEditor.Common.UpgradeModal as UpgradeModal
import Wizard.KMEditor.Editor.KMEditorRoute as KMEditorRoute
import Wizard.KMEditor.Index.Models exposing (Model)
import Wizard.KMEditor.Index.Msgs exposing (Msg(..))
import Wizard.KMEditor.Routes exposing (Route(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "KMEditor__Index" ]
        [ Page.header (gettext "Knowledge Model Editors" appState.locale) []
        , FormResult.view appState model.deletingMigration
        , Listing.view appState (listingConfig appState) model.branches
        , Html.map DeleteModalMsg <| DeleteModal.view appState model.deleteModal
        , Html.map UpgradeModalMsg <| UpgradeModal.view appState model.upgradeModal
        ]


createButton : AppState -> Html Msg
createButton appState =
    linkTo appState
        (Routes.kmEditorCreate Nothing Nothing)
        [ class "btn btn-primary"
        , dataCy "km-editor_create-button"
        ]
        [ text (gettext "Create" appState.locale) ]


listingConfig : AppState -> ViewConfig Branch Msg
listingConfig appState =
    { title = listingTitle appState
    , description = listingDescription appState
    , itemAdditionalData = always Nothing
    , dropdownItems = listingActions appState
    , textTitle = .name
    , emptyText = gettext "Click \"Create\" button to add a new knowledge model editor." appState.locale
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    , wrapMsg = ListingMsg
    , iconView = Nothing
    , searchPlaceholderText = Just (gettext "Search KM editors..." appState.locale)
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        , ( "createdAt", gettext "Created" appState.locale )
        , ( "updatedAt", gettext "Updated" appState.locale )
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
                linkTo appState (Routes.kmEditorMigration branch.uuid)

            else
                span

        BranchState.Migrated ->
            if Feature.knowledgeModelEditorPublish appState branch then
                linkTo appState (Routes.kmEditorPublish branch.uuid)

            else
                span

        _ ->
            linkTo appState (Routes.kmEditorEditor branch.uuid Nothing)


listingTitleLastPublishedVersionBadge : AppState -> Branch -> Html msg
listingTitleLastPublishedVersionBadge appState branch =
    let
        badge version =
            Badge.light (tooltip <| gettext "Last published version" appState.locale)
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
                ([ class Badge.warningClass
                 , onClick (UpgradeModalMsg (UpgradeModal.open branch.uuid branch.name (Maybe.withDefault "" branch.forkOfPackageId)))
                 , dataCy "km-editor_list_outdated-badge"
                 ]
                    ++ tooltip (gettext "There is a new version of parent knowledge model" appState.locale)
                )
                [ text (gettext "update available" appState.locale) ]

        BranchState.Migrating ->
            Badge.info
                (tooltip <| gettext "This editor is in the process of migration to a new parent knowledge model" appState.locale)
                [ text (gettext "migrating" appState.locale) ]

        BranchState.Migrated ->
            Badge.success
                (tooltip <| gettext "This editor has been migrated to a new parent knowledge model, you can publish it now" appState.locale)
                [ text (gettext "migrated" appState.locale) ]

        BranchState.Edited ->
            span (tooltip (gettext "This editor contains unpublished changes" appState.locale))
                [ faSet "kmEditorList.edited" appState ]

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
                                    linkTo appState (Routes.knowledgeModelsDetail <| orgId ++ ":" ++ kmId ++ ":" ++ version)

                                _ ->
                                    span
                    in
                    elem [ class "fragment", title <| gettext "Parent Knowledge Model" appState.locale ]
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
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmEditorList.edit" appState
                , label = gettext "Open Editor" appState.locale
                , msg = ListingActionLink (Routes.KMEditorRoute <| EditorRoute branch.uuid (KMEditorRoute.Edit Nothing))
                , dataCy = "open-editor"
                }

        upgrade =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmEditorList.update" appState
                , label = gettext "Update" appState.locale
                , msg = ListingActionMsg <| UpgradeModalMsg (UpgradeModal.open branch.uuid branch.name (Maybe.withDefault "" branch.forkOfPackageId))
                , dataCy = "update"
                }

        continueMigration =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "kmEditorList.continueMigration" appState
                , label = gettext "Continue migration" appState.locale
                , msg = ListingActionLink <| Routes.KMEditorRoute <| MigrationRoute branch.uuid
                , dataCy = "continue-migration"
                }

        cancelMigration =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faSet "_global.cancel" appState
                , label = gettext "Cancel migration" appState.locale
                , msg = ListingActionMsg <| DeleteMigration branch.uuid
                , dataCy = "cancel-migration"
                }

        delete =
            ListingDropdown.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faSet "_global.delete" appState
                , label = gettext "Delete" appState.locale
                , msg = ListingActionMsg <| DeleteModalMsg (DeleteModal.open branch.uuid branch.name)
                , dataCy = "delete-migration"
                }

        showOpenEditor =
            Feature.knowledgeModelEditorOpen appState branch

        showUpgrade =
            Feature.knowledgeModelEditorUpgrade appState branch

        showContinueMigration =
            Feature.knowledgeModelEditorContinueMigration appState branch

        showCancelMigration =
            Feature.knowledgeModelEditorCancelMigration appState branch

        showDelete =
            Feature.knowledgeModelEditorDelete appState branch

        groups =
            [ [ ( openEditor, showOpenEditor ) ]
            , [ ( upgrade, showUpgrade )
              , ( continueMigration, showContinueMigration )
              , ( cancelMigration, showCancelMigration )
              ]
            , [ ( delete, showDelete ) ]
            ]
    in
    ListingDropdown.itemsFromGroups groups
