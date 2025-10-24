module Wizard.Pages.KMEditor.Index.View exposing (view)

import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (faCancel, faDelete, faKmEditorListContinueMigration, faKmEditorListEdit, faKmEditorListEdited, faKmEditorListUpdate, faKmFork)
import Common.Components.FormResult as FormResult
import Common.Components.Page as Page
import Common.Components.Tooltip exposing (tooltip)
import Common.Utils.IdentifierUtils as IdentifierUtils
import Gettext exposing (gettext)
import Html exposing (Attribute, Html, a, code, div, span, text)
import Html.Attributes exposing (class, title)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Version
import Wizard.Api.Models.KnowledgeModelEditor exposing (KnowledgeModelEditor)
import Wizard.Api.Models.KnowledgeModelEditor.KnowledgeModelEditorState as KnowledgeModelEditorState
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.Listing.View as Listing exposing (ViewConfig)
import Wizard.Components.ListingDropdown as ListingDropdown exposing (ListingActionType(..), ListingDropdownItem)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KMEditor.Common.DeleteModal as DeleteModal
import Wizard.Pages.KMEditor.Common.KnowledgeModelEditorUtils as KnowledgeModelEditorUtils
import Wizard.Pages.KMEditor.Common.UpgradeModal as UpgradeModal
import Wizard.Pages.KMEditor.Editor.KMEditorRoute as KMEditorRoute
import Wizard.Pages.KMEditor.Index.Models exposing (Model)
import Wizard.Pages.KMEditor.Index.Msgs exposing (Msg(..))
import Wizard.Pages.KMEditor.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Utils.Feature as Feature
import Wizard.Utils.HtmlAttributesUtils exposing (listClass)


view : AppState -> Model -> Html Msg
view appState model =
    div [ listClass "KMEditor__Index" ]
        [ Page.header (gettext "Knowledge Model Editors" appState.locale) []
        , FormResult.view model.deletingMigration
        , Listing.view appState (listingConfig appState) model.kmEditors
        , Html.map DeleteModalMsg <| DeleteModal.view appState model.deleteModal
        , Html.map UpgradeModalMsg <| UpgradeModal.view appState model.upgradeModal
        ]


createButton : AppState -> Html Msg
createButton appState =
    linkTo (Routes.kmEditorCreate Nothing Nothing)
        [ class "btn btn-primary"
        , dataCy "km-editor_create-button"
        ]
        [ text (gettext "Create" appState.locale) ]


listingConfig : AppState -> ViewConfig KnowledgeModelEditor Msg
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


listingTitle : AppState -> KnowledgeModelEditor -> Html Msg
listingTitle appState kmEditor =
    span []
        [ linkToKM appState kmEditor [] [ text kmEditor.name ]
        , listingTitleLastPublishedVersionBadge appState kmEditor
        , listingTitleBadge appState kmEditor
        ]


linkToKM : AppState -> KnowledgeModelEditor -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
linkToKM appState kmEditor =
    case kmEditor.state of
        KnowledgeModelEditorState.Migrating ->
            if Feature.knowledgeModelEditorContinueMigration appState kmEditor then
                linkTo (Routes.kmEditorMigration kmEditor.uuid)

            else
                span

        KnowledgeModelEditorState.Migrated ->
            if Feature.knowledgeModelEditorPublish appState kmEditor then
                linkTo (Routes.kmEditorPublish kmEditor.uuid)

            else
                span

        _ ->
            linkTo (Routes.kmEditorEditor kmEditor.uuid Nothing)


listingTitleLastPublishedVersionBadge : AppState -> KnowledgeModelEditor -> Html msg
listingTitleLastPublishedVersionBadge appState kmEditor =
    let
        badge version =
            Badge.light (tooltip <| gettext "Last published version" appState.locale)
                [ text <| Version.toString version ]
    in
    KnowledgeModelEditorUtils.lastVersion appState kmEditor
        |> Maybe.map badge
        |> Maybe.withDefault Html.nothing


listingTitleBadge : AppState -> KnowledgeModelEditor -> Html Msg
listingTitleBadge appState kmEditor =
    case kmEditor.state of
        KnowledgeModelEditorState.Outdated ->
            a
                ([ class Badge.warningClass
                 , onClick (UpgradeModalMsg (UpgradeModal.open kmEditor.uuid kmEditor.name (Maybe.withDefault "" kmEditor.forkOfKnowledgeModelPackageId)))
                 , dataCy "km-editor_list_outdated-badge"
                 ]
                    ++ tooltip (gettext "There is a new version of parent knowledge model" appState.locale)
                )
                [ text (gettext "update available" appState.locale) ]

        KnowledgeModelEditorState.Migrating ->
            Badge.info
                (tooltip <| gettext "This editor is in the process of migration to a new parent knowledge model" appState.locale)
                [ text (gettext "migrating" appState.locale) ]

        KnowledgeModelEditorState.Migrated ->
            Badge.success
                (tooltip <| gettext "This editor has been migrated to a new parent knowledge model, you can publish it now" appState.locale)
                [ text (gettext "migrated" appState.locale) ]

        KnowledgeModelEditorState.Edited ->
            span (tooltip (gettext "This editor contains unpublished changes" appState.locale))
                [ faKmEditorListEdited ]

        _ ->
            Html.nothing


listingDescription : AppState -> KnowledgeModelEditor -> Html Msg
listingDescription appState kmEditor =
    let
        parent =
            case kmEditor.forkOfKnowledgeModelPackageId of
                Just forkOfPackageId ->
                    let
                        elem =
                            case IdentifierUtils.getComponents forkOfPackageId of
                                Just ( orgId, kmId, version ) ->
                                    linkTo (Routes.knowledgeModelsDetail <| orgId ++ ":" ++ kmId ++ ":" ++ version)

                                _ ->
                                    span
                    in
                    elem [ class "fragment", title <| gettext "Parent Knowledge Model" appState.locale ]
                        [ faKmFork
                        , text forkOfPackageId
                        ]

                Nothing ->
                    Html.nothing
    in
    span []
        [ span [ class "fragment" ] [ code [] [ text kmEditor.kmId ] ]
        , parent
        ]


listingActions : AppState -> KnowledgeModelEditor -> List (ListingDropdownItem Msg)
listingActions appState kmEditor =
    let
        openEditor =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faKmEditorListEdit
                , label = gettext "Open Editor" appState.locale
                , msg = ListingActionLink (Routes.KMEditorRoute <| EditorRoute kmEditor.uuid (KMEditorRoute.Edit Nothing))
                , dataCy = "open-editor"
                }

        upgrade =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faKmEditorListUpdate
                , label = gettext "Update" appState.locale
                , msg = ListingActionMsg <| UpgradeModalMsg (UpgradeModal.open kmEditor.uuid kmEditor.name (Maybe.withDefault "" kmEditor.forkOfKnowledgeModelPackageId))
                , dataCy = "update"
                }

        continueMigration =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faKmEditorListContinueMigration
                , label = gettext "Continue migration" appState.locale
                , msg = ListingActionLink <| Routes.KMEditorRoute <| MigrationRoute kmEditor.uuid
                , dataCy = "continue-migration"
                }

        cancelMigration =
            ListingDropdown.dropdownAction
                { extraClass = Nothing
                , icon = faCancel
                , label = gettext "Cancel migration" appState.locale
                , msg = ListingActionMsg <| DeleteMigration kmEditor.uuid
                , dataCy = "cancel-migration"
                }

        delete =
            ListingDropdown.dropdownAction
                { extraClass = Just "text-danger"
                , icon = faDelete
                , label = gettext "Delete" appState.locale
                , msg = ListingActionMsg <| DeleteModalMsg (DeleteModal.open kmEditor.uuid kmEditor.name)
                , dataCy = "delete-migration"
                }

        showOpenEditor =
            Feature.knowledgeModelEditorOpen appState kmEditor

        showUpgrade =
            Feature.knowledgeModelEditorUpgrade appState kmEditor

        showContinueMigration =
            Feature.knowledgeModelEditorContinueMigration appState kmEditor

        showCancelMigration =
            Feature.knowledgeModelEditorCancelMigration appState kmEditor

        showDelete =
            Feature.knowledgeModelEditorDelete appState kmEditor

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
