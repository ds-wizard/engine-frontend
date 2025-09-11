module Wizard.Pages.KMEditor.Index.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Task.Extra as Task
import Uuid exposing (Uuid)
import Wizard.Api.Branches as BranchesApi
import Wizard.Api.Models.Branch exposing (Branch)
import Wizard.Components.Listing.Msgs as ListingMsgs
import Wizard.Components.Listing.Update as Listing
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KMEditor.Common.DeleteModal as DeleteModal
import Wizard.Pages.KMEditor.Common.UpgradeModal as UpgradeModal
import Wizard.Pages.KMEditor.Index.Models exposing (Model)
import Wizard.Pages.KMEditor.Index.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        DeleteMigration uuid ->
            handleDeleteMigration wrapMsg appState model uuid

        DeleteMigrationCompleted result ->
            handleDeleteMigrationCompleted wrapMsg appState model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model

        DeleteModalMsg deleteModalMsg ->
            handleDeleteModalMsg wrapMsg appState deleteModalMsg model

        UpgradeModalMsg upgradeModalMsg ->
            handleUpgradeModalMsg wrapMsg appState upgradeModalMsg model



-- Handlers


handleDeleteMigration : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Uuid -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteMigration wrapMsg appState model uuid =
    ( { model | deletingMigration = Loading }
    , Cmd.map wrapMsg <| BranchesApi.deleteMigration appState uuid DeleteMigrationCompleted
    )


handleDeleteMigrationCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteMigrationCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            let
                ( branches, cmd ) =
                    Listing.update (listingUpdateConfig wrapMsg appState) appState ListingMsgs.Reload model.branches
            in
            ( { model
                | deletingMigration = Success <| gettext "Migration was successfully canceled." appState.locale
                , branches = branches
              }
            , cmd
            )

        Err error ->
            ( { model | deletingMigration = ApiError.toActionResult appState (gettext "Migration could not be cancelled." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg Branch -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( branches, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState) appState listingMsg model.branches
    in
    ( { model | branches = branches }
    , cmd
    )


handleDeleteModalMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> DeleteModal.Msg -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteModalMsg wrapMsg appState deleteModalMsg model =
    let
        updateConfig =
            { cmdDeleted = Task.dispatch (wrapMsg (ListingMsg ListingMsgs.OnAfterDelete))
            , wrapMsg = wrapMsg << DeleteModalMsg
            }

        ( deleteModal, cmd ) =
            DeleteModal.update updateConfig appState deleteModalMsg model.deleteModal
    in
    ( { model | deleteModal = deleteModal }, cmd )


handleUpgradeModalMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> UpgradeModal.Msg -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleUpgradeModalMsg wrapMsg appState upgradeModalMsg model =
    let
        updateConfig =
            { cmdUpgraded = cmdNavigate appState << Routes.kmEditorMigration
            , wrapMsg = wrapMsg << UpgradeModalMsg
            }

        ( upgradeModal, cmd ) =
            UpgradeModal.update updateConfig appState upgradeModalMsg model.upgradeModal
    in
    ( { model | upgradeModal = upgradeModal }, cmd )



-- Utils


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig Branch
listingUpdateConfig wrapMsg appState =
    { getRequest = BranchesApi.getBranches appState
    , getError = gettext "Unable to get knowledge model editors." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.kmEditorIndexWithFilters
    }
