module Wizard.KMEditor.Index.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Shared.Api.Branches as BranchesApi
import Shared.Data.Branch exposing (Branch)
import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setPackage)
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.KMEditor.Common.DeleteModal as DeleteModal
import Wizard.KMEditor.Common.UpgradeModal as UpgradeModal
import Wizard.KMEditor.Index.Models exposing (Model)
import Wizard.KMEditor.Index.Msgs exposing (Msg(..))
import Wizard.KMEditor.Routes exposing (Route(..))
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetPackageCompleted result ->
            handleGetPackageCompleted appState model result

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


handleGetPackageCompleted : AppState -> Model -> Result ApiError PackageDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetPackageCompleted appState model result =
    applyResult appState
        { setResult = setPackage
        , defaultError = lg "apiError.packages.getError" appState
        , model = model
        , result = result
        }


handleDeleteMigration : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Uuid -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteMigration wrapMsg appState model uuid =
    ( { model | deletingMigration = Loading }
    , Cmd.map wrapMsg <| BranchesApi.deleteMigration uuid appState DeleteMigrationCompleted
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
                | deletingMigration = Success <| lg "apiSuccess.migration.delete" appState
                , branches = branches
              }
            , cmd
            )

        Err error ->
            ( { model | deletingMigration = ApiError.toActionResult appState (lg "apiError.branches.migrations.deleteError" appState) error }
            , getResultCmd result
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
            { cmdDeleted = cmdNavigate appState Routes.kmEditorIndex
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
            { cmdUpgraded = cmdNavigate appState << Routes.KMEditorRoute << MigrationRoute
            , wrapMsg = wrapMsg << UpgradeModalMsg
            }

        ( upgradeModal, cmd ) =
            UpgradeModal.update updateConfig appState upgradeModalMsg model.upgradeModal
    in
    ( { model | upgradeModal = upgradeModal }, cmd )



-- Utils


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig Branch
listingUpdateConfig wrapMsg appState =
    { getRequest = BranchesApi.getBranches
    , getError = lg "apiError.branches.getListError" appState
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.KMEditorRoute << IndexRoute
    }
