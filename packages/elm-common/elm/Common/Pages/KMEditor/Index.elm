module Common.Pages.KMEditor.Index exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| The list of knowledge model editors.

Each row links to where the editor can be continued — the editor, the migration
while one is running, or publishing once it has finished — and says its state in
a badge. The actions are open, update, continue and cancel migration, and
delete, plus "Publish" for the states an editor can be published from.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom as Dom
import Browser.Navigation as Navigation
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Common.Api.Models.KnowledgeModelEditor exposing (KnowledgeModelEditor)
import Common.Api.Models.KnowledgeModelEditor.KnowledgeModelEditorState as KnowledgeModelEditorState
import Common.Components.ItemActions as ItemActions
import Common.Components.Listing as Listing
import Common.Components.Timestamp as Timestamp
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.KMEditor.Components.DeleteModal as DeleteModal
import Common.Pages.KMEditor.Components.UpgradeModal as UpgradeModal
import Common.Routes as Routes
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html, a, span, text)
import Html.Attributes exposing (href)
import Maybe.Extra as Maybe
import String.Format as String
import Task
import Task.Extra as Task
import Ui.Alert as Alert
import Ui.AppShell as AppShell
import Ui.Badge as Badge exposing (Badge)
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.ItemIcon as ItemIcon
import Ui.Layout as Layout
import Ui.Text as Text
import Ui.Tooltip as Tooltip
import Uuid exposing (Uuid)



-- MODEL


type alias Model =
    { kmEditors : Listing.Model KnowledgeModelEditor
    , deletingMigration : ActionResult String
    , deleteModal : DeleteModal.Model
    , upgradeModal : UpgradeModal.Model
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { kmEditors = Listing.initialModel paginationQueryString
    , deletingMigration = Unset
    , deleteModal = DeleteModal.initialModel
    , upgradeModal = UpgradeModal.initialModel
    }


init : PaginationQueryString -> ( Model, Cmd Msg )
init paginationQueryString =
    let
        ( kmEditors, listingCmd ) =
            Listing.init paginationQueryString PaginationQueryFilters.empty

        model =
            initialModel paginationQueryString
    in
    ( { model | kmEditors = kmEditors }
    , Cmd.map ListingMsg listingCmd
    )



-- UPDATE


type Msg
    = DeleteMigration KnowledgeModelEditor
    | DeleteMigrationCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg KnowledgeModelEditor)
    | DeleteModalMsg DeleteModal.Msg
    | UpgradeModalMsg UpgradeModal.Msg
    | OpenDialog Msg
    | NoOp


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        DeleteMigration kmEditor ->
            ( { model | deletingMigration = Loading }
            , Cmd.map cfg.wrapMsg <| KnowledgeModelEditorsApi.deleteMigration appState kmEditor.uuid DeleteMigrationCompleted
            )

        DeleteMigrationCompleted result ->
            case result of
                Ok _ ->
                    let
                        ( kmEditors, listingCmd ) =
                            Listing.update appState (listingUpdateConfig cfg appState) Listing.reload model.kmEditors
                    in
                    -- The action that was focused is gone with the reloaded
                    -- rows, so focus goes to the heading, above the result.
                    ( { model
                        | deletingMigration = Success <| gettext "Migration was successfully canceled." appState.locale
                        , kmEditors = kmEditors
                      }
                    , Cmd.batch [ listingCmd, AppShell.focusOnRouteChange (cfg.wrapMsg NoOp) ]
                    )

                Err error ->
                    ( { model | deletingMigration = ApiError.toActionResult appState (gettext "Migration could not be canceled." appState.locale) error }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , AppShell.focusOnRouteChange (cfg.wrapMsg NoOp)
                        ]
                    )

        ListingMsg listingMsg ->
            let
                ( kmEditors, cmd ) =
                    Listing.update appState (listingUpdateConfig cfg appState) listingMsg model.kmEditors
            in
            ( { model | kmEditors = kmEditors }, cmd )

        DeleteModalMsg deleteModalMsg ->
            let
                updateConfig =
                    { cmdDeleted = Task.dispatch (cfg.wrapMsg (ListingMsg Listing.onAfterDelete))
                    , cmdClosed = focusRowActions cfg
                    , wrapMsg = cfg.wrapMsg << DeleteModalMsg
                    }

                ( deleteModal, cmd ) =
                    DeleteModal.update updateConfig appState deleteModalMsg model.deleteModal
            in
            ( { model | deleteModal = deleteModal }, cmd )

        UpgradeModalMsg upgradeModalMsg ->
            let
                updateConfig =
                    { cmdUpgraded = Navigation.pushUrl appState.key << Routes.toUrl appState << Routes.kmEditorMigration
                    , cmdClosed = focusRowActions cfg
                    , wrapMsg = cfg.wrapMsg << UpgradeModalMsg
                    }

                ( upgradeModal, cmd ) =
                    UpgradeModal.update updateConfig appState upgradeModalMsg model.upgradeModal
            in
            ( { model | upgradeModal = upgradeModal }, cmd )

        OpenDialog dialogMsg ->
            -- The dialog takes focus as it opens; the row's menu closes
            -- without taking it back.
            let
                ( kmEditors, listingCmd ) =
                    Listing.update appState (listingUpdateConfig cfg appState) Listing.closeMenus model.kmEditors

                ( newModel, dialogCmd ) =
                    update cfg appState dialogMsg { model | kmEditors = kmEditors }
            in
            ( newModel, Cmd.batch [ listingCmd, dialogCmd ] )

        NoOp ->
            ( model, Cmd.none )


{-| A dialog a row's action opened was closed and the row is still there: focus
goes back to the row's menu.
-}
focusRowActions : UpdateConfig Msg msg -> Uuid -> Cmd msg
focusRowActions cfg uuid =
    Task.attempt (always (cfg.wrapMsg NoOp)) (Dom.focus (Listing.itemActionsTriggerId (Uuid.toString uuid)))


listingUpdateConfig : UpdateConfig Msg msg -> AppState -> Listing.UpdateConfig KnowledgeModelEditor msg
listingUpdateConfig cfg appState =
    { getRequest = KnowledgeModelEditorsApi.getKnowledgeModelEditors appState
    , getError = gettext "Unable to get knowledge model editors." appState.locale
    , wrapMsg = cfg.wrapMsg << ListingMsg
    , logoutMsg = cfg.logoutMsg
    , toUrl = \filters pqs -> Routes.toUrl appState (Routes.kmEditorIndexWithFilters filters pqs)
    , key = appState.key
    , validateFilters = Nothing
    , searchFilters = []
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg (Listing.subscriptions model.kmEditors)



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Listing.viewWith appState
        { notices = migrationResult model.deletingMigration
        , overlays =
            [ Html.map DeleteModalMsg <| DeleteModal.view appState model.deleteModal
            , Html.map UpgradeModalMsg <| UpgradeModal.view appState model.upgradeModal
            ]
        }
        (listingConfig appState)
        model.kmEditors


listingConfig : AppState -> Listing.ViewConfig KnowledgeModelEditor Msg
listingConfig appState =
    { title = gettext "Knowledge Model Editors" appState.locale
    , description = Nothing
    , breadcrumbs = Nothing
    , toolbarActions =
        [ Button.linkPrimary (gettext "Create" appState.locale) (Routes.toUrl appState (Routes.kmEditorCreate Nothing Nothing))
            |> Button.withIcon (Icon.solid "fa-plus")
            |> Button.withTestId "km-editor_create-button"
        ]
    , searchPlaceholder = gettext "Search KM editors..." appState.locale
    , sortOptions =
        [ ( "name", gettext "Name" appState.locale )
        , ( "createdAt", gettext "Created" appState.locale )
        , ( "updatedAt", gettext "Updated" appState.locale )
        ]
    , filters = []
    , item = listingItem appState
    , emptyTitle = gettext "No data" appState.locale
    , emptyDescription = gettext "Click \"Create\" button to add a new knowledge model editor." appState.locale
    , wrapMsg = ListingMsg
    }


{-| The result of canceling a migration, above the list.
-}
migrationResult : ActionResult String -> List (Html msg)
migrationResult result =
    case result of
        Success message ->
            [ Alert.toHtml (Alert.success message) ]

        Error message ->
            [ Alert.toHtml (Alert.danger message) ]

        _ ->
            []


listingItem : AppState -> KnowledgeModelEditor -> Listing.Item Msg
listingItem appState kmEditor =
    let
        item =
            Listing.item
                { id = Uuid.toString kmEditor.uuid
                , title = listingTitle appState kmEditor
                , actionsLabel = String.format (gettext "Actions for %s" appState.locale) [ kmEditor.name ]
                }
    in
    { item
        | icon = Just (ItemIcon.toHtml (UserAvatar.item kmEditor.name Nothing))
        , badges = listingBadges appState kmEditor
        , fragments = listingFragments appState kmEditor
        , timestamp = Just (Timestamp.updated appState kmEditor.updatedAt)
        , actions = listingActions appState kmEditor
    }


{-| The name, linked to where the editor can be continued, and the pen of an
editor with unpublished changes. The pen is a labeled image, so the state is
said as well as shown.
-}
listingTitle : AppState -> KnowledgeModelEditor -> Html Msg
listingTitle appState kmEditor =
    let
        name =
            case titleUrl appState kmEditor of
                Just url ->
                    a [ href url ] [ text kmEditor.name ]

                Nothing ->
                    span [] [ text kmEditor.name ]
    in
    if kmEditor.state == KnowledgeModelEditorState.Edited then
        let
            editedLabel =
                gettext "This editor contains unpublished changes" appState.locale
        in
        Layout.inline
            [ name
            , Tooltip.view { label = editedLabel, placement = Tooltip.Below }
                (Icon.toHtml (Icon.labeled editedLabel (Icon.solid "fa-pen")))
            ]

    else
        name


titleUrl : AppState -> KnowledgeModelEditor -> Maybe String
titleUrl appState kmEditor =
    case kmEditor.state of
        KnowledgeModelEditorState.Migrating ->
            if Feature.knowledgeModelEditorContinueMigration appState kmEditor then
                Just (Routes.toUrl appState (Routes.kmEditorMigration kmEditor.uuid))

            else
                Nothing

        KnowledgeModelEditorState.Migrated ->
            if Feature.knowledgeModelEditorPublish appState kmEditor then
                Just (Routes.toUrl appState (Routes.kmEditorPublish kmEditor.uuid))

            else
                Nothing

        _ ->
            Just (Routes.toUrl appState (Routes.kmEditorEditor kmEditor.uuid Nothing))


{-| Migrating this editor onto a newer parent knowledge model, which is asked
about in a dialog. Both the badge that says there is one and the action in the
menu open it.
-}
openUpgrade : KnowledgeModelEditor -> Msg
openUpgrade kmEditor =
    OpenDialog (UpgradeModalMsg (UpgradeModal.open kmEditor.uuid kmEditor.name (Maybe.unwrap Uuid.nil .uuid kmEditor.forkOfPackage)))


listingBadges : AppState -> KnowledgeModelEditor -> List (Badge Msg)
listingBadges appState kmEditor =
    case kmEditor.state of
        KnowledgeModelEditorState.Outdated ->
            if Maybe.isJust kmEditor.forkOfPackage then
                -- The badge is the whole reason to open the dialog, so it
                -- opens it: the same thing the Update action does, where the
                -- reader is already looking.
                [ Badge.warning (gettext "update available" appState.locale)
                    |> Badge.withDescription (gettext "There is a new version of parent knowledge model" appState.locale)
                    |> Badge.withAction (openUpgrade kmEditor)
                ]

            else
                []

        KnowledgeModelEditorState.Migrating ->
            [ Badge.info (gettext "migrating" appState.locale)
                |> Badge.withDescription (gettext "This editor is in the process of migration to a new parent knowledge model" appState.locale)
            ]

        KnowledgeModelEditorState.Migrated ->
            [ Badge.success (gettext "migrated" appState.locale)
                |> Badge.withDescription (gettext "This editor has been migrated to a new parent knowledge model, you can publish it now" appState.locale)
            ]

        _ ->
            []


listingFragments : AppState -> KnowledgeModelEditor -> List (Html Msg)
listingFragments appState kmEditor =
    let
        parent =
            case kmEditor.forkOfPackage of
                Just forkOfPackage ->
                    [ span []
                        [ Icon.toHtml (Icon.labeled (gettext "Parent Knowledge Model" appState.locale) (Icon.solid "fa-code-branch"))
                        , text " "
                        , text (KnowledgeModelUtils.getPackageId forkOfPackage)
                        ]
                    ]

                Nothing ->
                    []
    in
    Text.code kmEditor.id :: parent


listingActions : AppState -> KnowledgeModelEditor -> List (ItemActions.Action Msg)
listingActions appState kmEditor =
    let
        openEditor =
            ItemActions.link
                { icon = Icon.solid "fa-pen"
                , label = gettext "Open Editor" appState.locale
                , url = Routes.toUrl appState (Routes.kmEditorEditor kmEditor.uuid Nothing)
                }
                |> ItemActions.withTestId "listing-item_action_open-editor"

        publish =
            ItemActions.link
                { icon = Icon.solid "fa-cloud-arrow-up"
                , label = gettext "Publish" appState.locale
                , url = Routes.toUrl appState (Routes.kmEditorPublish kmEditor.uuid)
                }
                |> ItemActions.withTestId "listing-item_action_publish"

        upgrade =
            ItemActions.action
                { icon = Icon.solid "fa-arrow-up-wide-short"
                , label = gettext "Update" appState.locale
                , onSelect = openUpgrade kmEditor
                }
                |> ItemActions.withTestId "listing-item_action_update"

        continueMigration =
            ItemActions.link
                { icon = Icon.solid "fa-arrow-right-long"
                , label = gettext "Continue migration" appState.locale
                , url = Routes.toUrl appState (Routes.kmEditorMigration kmEditor.uuid)
                }
                |> ItemActions.withTestId "listing-item_action_continue-migration"

        cancelMigration =
            ItemActions.action
                { icon = Icon.solid "fa-ban"
                , label = gettext "Cancel migration" appState.locale
                , onSelect = DeleteMigration kmEditor
                }
                |> ItemActions.withTestId "listing-item_action_cancel-migration"

        delete =
            ItemActions.dangerAction
                { icon = Icon.solid "fa-trash"
                , label = gettext "Delete" appState.locale
                , onSelect = OpenDialog (DeleteModalMsg (DeleteModal.open kmEditor))
                }
                |> ItemActions.withTestId "listing-item_action_delete-migration"
    in
    ItemActions.groups
        [ [ ( openEditor, Feature.knowledgeModelEditorOpen appState kmEditor )
          , ( publish, Feature.knowledgeModelEditorPublish appState kmEditor )
          ]
        , [ ( upgrade, Feature.knowledgeModelEditorUpgrade appState kmEditor )
          , ( continueMigration, Feature.knowledgeModelEditorContinueMigration appState kmEditor )
          , ( cancelMigration, Feature.knowledgeModelEditorCancelMigration appState kmEditor )
          ]
        , [ ( delete, Feature.knowledgeModelEditorDelete appState ) ]
        ]
