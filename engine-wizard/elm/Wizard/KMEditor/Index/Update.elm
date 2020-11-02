module Wizard.KMEditor.Index.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Form
import Maybe.Extra as Maybe
import Shared.Api.Branches as BranchesApi
import Shared.Api.Packages as PackagesApi
import Shared.Data.Branch exposing (Branch)
import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (l, lg)
import Shared.Setters exposing (setPackage)
import Shared.Utils exposing (withNoCmd)
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.KMEditor.Common.BranchUpgradeForm as BranchUpgradeForm
import Wizard.KMEditor.Index.Models exposing (Model)
import Wizard.KMEditor.Index.Msgs exposing (Msg(..))
import Wizard.KMEditor.Routes exposing (Route(..))
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Index.Update"


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ShowHideDeleteBranchModal branch ->
            handleShowHideDeleteBranchModal model branch

        DeleteBranch ->
            handleDeleteBranch wrapMsg appState model

        DeleteBranchCompleted result ->
            handleDeleteBranchCompleted appState model result

        PostMigrationCompleted result ->
            handlePostMigrationCompleted appState model result

        ShowHideUpgradeModal mbBranch ->
            handleShowHideUpgradeModal wrapMsg appState model mbBranch

        UpgradeFormMsg formMsg ->
            handleUpgradeFormMsg formMsg wrapMsg appState model

        GetPackageCompleted result ->
            handleGetPackageCompleted appState model result

        DeleteMigration uuid ->
            handleDeleteMigration wrapMsg appState model uuid

        DeleteMigrationCompleted result ->
            handleDeleteMigrationCompleted wrapMsg appState model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model



-- Handlers


handleShowHideDeleteBranchModal : Model -> Maybe Branch -> ( Model, Cmd Wizard.Msgs.Msg )
handleShowHideDeleteBranchModal model mbBranch =
    withNoCmd <|
        { model
            | branchToBeDeleted = mbBranch
            , deletingKnowledgeModel = Unset
        }


handleDeleteBranch : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteBranch wrapMsg appState model =
    case model.branchToBeDeleted of
        Just branch ->
            ( { model | deletingKnowledgeModel = Loading }
            , Cmd.map wrapMsg <| BranchesApi.deleteBranch branch.uuid appState DeleteBranchCompleted
            )

        _ ->
            withNoCmd model


handleDeleteBranchCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteBranchCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState Routes.kmEditorIndex )

        Err error ->
            ( { model | deletingKnowledgeModel = ApiError.toActionResult (lg "apiError.branches.deleteError" appState) error }
            , getResultCmd result
            )


handlePostMigrationCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostMigrationCompleted appState model result =
    case result of
        Ok _ ->
            let
                kmUuid =
                    Maybe.unwrap Uuid.nil .uuid model.branchToBeUpgraded
            in
            ( model, cmdNavigate appState <| Routes.KMEditorRoute <| MigrationRoute kmUuid )

        Err error ->
            ( { model | creatingMigration = ApiError.toActionResult (lg "apiError.branches.migrations.postError" appState) error }
            , getResultCmd result
            )


handleShowHideUpgradeModal : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Maybe Branch -> ( Model, Cmd Wizard.Msgs.Msg )
handleShowHideUpgradeModal wrapMsg appState model mbBranch =
    let
        getPackages lastAppliedParentPackageId =
            let
                cmd =
                    Cmd.map wrapMsg <|
                        PackagesApi.getPackage lastAppliedParentPackageId appState GetPackageCompleted
            in
            Just ( { model | branchToBeUpgraded = mbBranch, package = Loading }, cmd )
    in
    mbBranch
        |> Maybe.andThen .forkOfPackageId
        |> Maybe.andThen getPackages
        |> Maybe.withDefault ( { model | branchToBeUpgraded = Nothing, package = Unset }, Cmd.none )


handleUpgradeFormMsg : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleUpgradeFormMsg formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.branchUpgradeForm, model.branchToBeUpgraded ) of
        ( Form.Submit, Just branchUpgradeForm, Just branch ) ->
            let
                body =
                    BranchUpgradeForm.encode branchUpgradeForm

                cmd =
                    Cmd.map wrapMsg <|
                        BranchesApi.postMigration branch.uuid body appState PostMigrationCompleted
            in
            ( { model | creatingMigration = Loading }
            , cmd
            )

        _ ->
            withNoCmd <|
                { model | branchUpgradeForm = Form.update BranchUpgradeForm.validation formMsg model.branchUpgradeForm }


handleGetPackageCompleted : AppState -> Model -> Result ApiError PackageDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetPackageCompleted appState model result =
    applyResult
        { setResult = setPackage
        , defaultError = lg "apiError.packages.getError" appState
        , model = model
        , result = result
        }


handleDeleteMigration : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Uuid -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteMigration wrapMsg appState model uuid =
    ( { model | deletingMigration = Loading }
    , Cmd.map wrapMsg <| BranchesApi.deleteMigration uuid appState DeleteBranchCompleted
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
            ( { model | deletingMigration = ApiError.toActionResult (lg "apiError.branches.migrations.deleteError" appState) error }
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



-- Utils


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig Branch
listingUpdateConfig wrapMsg appState =
    { getRequest = BranchesApi.getBranches
    , getError = lg "apiError.branches.getListError" appState
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.KMEditorRoute << IndexRoute
    }
