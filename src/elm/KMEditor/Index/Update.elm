module KMEditor.Index.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (applyResult, getResultCmd)
import Common.Api.Branches as BranchesApi
import Common.Api.Packages as PackagesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Setters exposing (setBranches, setPackage)
import Form
import KMEditor.Common.Branch exposing (Branch)
import KMEditor.Common.BranchUpgradeForm as BranchUpgradeForm
import KMEditor.Index.Models exposing (Model)
import KMEditor.Index.Msgs exposing (Msg(..))
import KMEditor.Routing exposing (Route(..))
import KnowledgeModels.Common.PackageDetail exposing (PackageDetail)
import Msgs
import Routing exposing (Route(..), cmdNavigate)
import Utils exposing (withNoCmd)


fetchData : (Msg -> Msgs.Msg) -> AppState -> Cmd Msgs.Msg
fetchData wrapMsg appState =
    Cmd.map wrapMsg <|
        BranchesApi.getBranches appState GetBranchesCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetBranchesCompleted result ->
            handleGetBranchesCompleted model result

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
            handleGetPackagesCompleted model result

        DeleteMigration uuid ->
            handleDeleteMigration wrapMsg appState model uuid

        DeleteMigrationCompleted result ->
            handleDeleteMigrationCompleted wrapMsg appState model result



-- Handlers


handleGetBranchesCompleted : Model -> Result ApiError (List Branch) -> ( Model, Cmd Msgs.Msg )
handleGetBranchesCompleted model result =
    applyResult
        { setResult = setBranches
        , defaultError = "Unable to get knowledge model editors."
        , model = model
        , result = result
        }


handleShowHideDeleteBranchModal : Model -> Maybe Branch -> ( Model, Cmd Msgs.Msg )
handleShowHideDeleteBranchModal model mbBranch =
    withNoCmd <|
        { model
            | branchToBeDeleted = mbBranch
            , deletingKnowledgeModel = Unset
        }


handleDeleteBranch : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteBranch wrapMsg appState model =
    case model.branchToBeDeleted of
        Just branch ->
            ( { model | deletingKnowledgeModel = Loading }
            , Cmd.map wrapMsg <| BranchesApi.deleteBranch branch.uuid appState DeleteBranchCompleted
            )

        _ ->
            withNoCmd model


handleDeleteBranchCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
handleDeleteBranchCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState.key <| KMEditor IndexRoute )

        Err error ->
            ( { model | deletingKnowledgeModel = getServerError error "Knowledge model could not be deleted" }
            , getResultCmd result
            )


handlePostMigrationCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
handlePostMigrationCompleted appState model result =
    case result of
        Ok _ ->
            let
                kmUuid =
                    model.branchToBeUpgraded
                        |> Maybe.andThen (\branch -> Just branch.uuid)
                        |> Maybe.withDefault ""
            in
            ( model, cmdNavigate appState.key <| KMEditor <| MigrationRoute kmUuid )

        Err error ->
            ( { model | creatingMigration = getServerError error "Migration could not be created" }
            , getResultCmd result
            )


handleShowHideUpgradeModal : (Msg -> Msgs.Msg) -> AppState -> Model -> Maybe Branch -> ( Model, Cmd Msgs.Msg )
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


handleUpgradeFormMsg : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
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


handleGetPackagesCompleted : Model -> Result ApiError PackageDetail -> ( Model, Cmd Msgs.Msg )
handleGetPackagesCompleted model result =
    applyResult
        { setResult = setPackage
        , defaultError = "Unable to get knowledge model list."
        , model = model
        , result = result
        }


handleDeleteMigration : (Msg -> Msgs.Msg) -> AppState -> Model -> String -> ( Model, Cmd Msgs.Msg )
handleDeleteMigration wrapMsg appState model uuid =
    ( { model | deletingMigration = Loading }
    , Cmd.map wrapMsg <| BranchesApi.deleteMigration uuid appState DeleteBranchCompleted
    )


handleDeleteMigrationCompleted : (Msg -> Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
handleDeleteMigrationCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            ( { model | deletingMigration = Success "Migration was successfully canceled", branches = Loading }
            , fetchData wrapMsg appState
            )

        Err error ->
            ( { model | deletingMigration = getServerError error "Migration could not be deleted" }
            , getResultCmd result
            )
