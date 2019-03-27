module KMEditor.Index.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (Session)
import Common.Api exposing (getResultCmd)
import Common.Api.KnowledgeModels as KnowledgeModelsApi
import Common.Api.Packages as PackagesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Form
import KMEditor.Common.Models exposing (KnowledgeModel)
import KMEditor.Index.Models exposing (KnowledgeModelUpgradeForm, Model, encodeKnowledgeModelUpgradeForm, knowledgeModelUpgradeFormValidation)
import KMEditor.Index.Msgs exposing (Msg(..))
import KMEditor.Routing exposing (Route(..))
import KnowledgeModels.Common.Models exposing (PackageDetail)
import List.Extra as List
import Msgs
import Routing exposing (Route(..), cmdNavigate)
import Utils exposing (versionIsGreater)


fetchData : (Msg -> Msgs.Msg) -> AppState -> Cmd Msgs.Msg
fetchData wrapMsg appState =
    Cmd.map wrapMsg <|
        KnowledgeModelsApi.getKnowledgeModels appState GetKnowledgeModelsCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetKnowledgeModelsCompleted result ->
            getKnowledgeModelsCompleted model result

        ShowHideDeleteKnowledgeModal km ->
            ( { model | kmToBeDeleted = km, deletingKnowledgeModel = Unset }, Cmd.none )

        DeleteKnowledgeModel ->
            handleDeleteKM wrapMsg appState model

        DeleteKnowledgeModelCompleted result ->
            deleteKnowledgeModelCompleted appState model result

        PostMigrationCompleted result ->
            postMigrationCompleted appState model result

        ShowHideUpgradeModal km ->
            handleShowHideUpgradeModal wrapMsg km model appState

        UpgradeFormMsg formMsg ->
            handleUpgradeForm formMsg wrapMsg appState model

        GetPackagesCompleted result ->
            handleGetPackagesCompleted model result

        DeleteMigration uuid ->
            handleDeleteMigration wrapMsg uuid appState model

        DeleteMigrationCompleted result ->
            deleteMigrationCompleted wrapMsg appState model result


getKnowledgeModelsCompleted : Model -> Result ApiError (List KnowledgeModel) -> ( Model, Cmd Msgs.Msg )
getKnowledgeModelsCompleted model result =
    let
        newModel =
            case result of
                Ok knowledgeModels ->
                    { model | knowledgeModels = Success knowledgeModels }

                Err error ->
                    { model | knowledgeModels = getServerError error "Unable to fetch knowledge models" }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


handleDeleteKM : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteKM wrapMsg appState model =
    case model.kmToBeDeleted of
        Just km ->
            ( { model | deletingKnowledgeModel = Loading }
            , Cmd.map wrapMsg <| KnowledgeModelsApi.deleteKnowledgeModel km.uuid appState DeleteKnowledgeModelCompleted
            )

        _ ->
            ( model, Cmd.none )


deleteKnowledgeModelCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
deleteKnowledgeModelCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState.key <| KMEditor IndexRoute )

        Err error ->
            ( { model | deletingKnowledgeModel = getServerError error "Knowledge model could not be deleted" }
            , getResultCmd result
            )


postMigrationCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
postMigrationCompleted appState model result =
    case result of
        Ok migration ->
            let
                kmUuid =
                    model.kmToBeUpgraded
                        |> Maybe.andThen (\km -> Just km.uuid)
                        |> Maybe.withDefault ""
            in
            ( model, cmdNavigate appState.key <| KMEditor <| MigrationRoute kmUuid )

        Err error ->
            ( { model | creatingMigration = getServerError error "Migration could not be created" }
            , getResultCmd result
            )


handleShowHideUpgradeModal : (Msg -> Msgs.Msg) -> Maybe KnowledgeModel -> Model -> AppState -> ( Model, Cmd Msgs.Msg )
handleShowHideUpgradeModal wrapMsg maybeKm model appState =
    let
        getPackages lastAppliedParentPackageId =
            let
                cmd =
                    getPackagesFilteredCmd wrapMsg lastAppliedParentPackageId appState
            in
            Just ( { model | kmToBeUpgraded = maybeKm, packages = Loading }, cmd )
    in
    maybeKm
        |> Maybe.andThen .lastAppliedParentPackageId
        |> Maybe.andThen getPackages
        |> Maybe.withDefault ( { model | kmToBeUpgraded = Nothing, packages = Unset }, Cmd.none )


getPackagesFilteredCmd : (Msg -> Msgs.Msg) -> String -> AppState -> Cmd Msgs.Msg
getPackagesFilteredCmd wrapMsg lastAppliedParentPackageId appState =
    let
        parts =
            String.split ":" lastAppliedParentPackageId
    in
    case ( List.head parts, List.getAt 1 parts ) of
        ( Just organizationId, Just kmId ) ->
            Cmd.map wrapMsg <|
                PackagesApi.getPackagesFiltered organizationId kmId appState GetPackagesCompleted

        _ ->
            Cmd.none


handleUpgradeForm : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleUpgradeForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.kmUpgradeForm, model.kmToBeUpgraded ) of
        ( Form.Submit, Just kmUpgradeForm, Just km ) ->
            let
                body =
                    encodeKnowledgeModelUpgradeForm kmUpgradeForm

                cmd =
                    Cmd.map wrapMsg <|
                        KnowledgeModelsApi.postMigration km.uuid body appState PostMigrationCompleted
            in
            ( { model | creatingMigration = Loading }
            , cmd
            )

        _ ->
            ( { model | kmUpgradeForm = Form.update knowledgeModelUpgradeFormValidation formMsg model.kmUpgradeForm }
            , Cmd.none
            )


handleGetPackagesCompleted : Model -> Result ApiError (List PackageDetail) -> ( Model, Cmd Msgs.Msg )
handleGetPackagesCompleted model result =
    case result of
        Ok packages ->
            let
                packageList =
                    model.kmToBeUpgraded
                        |> Maybe.andThen (filterPackages packages)
                        |> Maybe.withDefault []
            in
            ( { model | packages = Success packageList }, Cmd.none )

        Err error ->
            ( { model | packages = getServerError error "Unable to get package list" }
            , getResultCmd result
            )


filterPackages : List PackageDetail -> KnowledgeModel -> Maybe (List PackageDetail)
filterPackages packageList knowledgeModel =
    let
        getFilteredList packages version =
            Just <| List.filter (.version >> versionIsGreater version) packages
    in
    knowledgeModel.lastAppliedParentPackageId
        |> Maybe.andThen (String.split ":" >> List.getAt 2)
        |> Maybe.andThen (getFilteredList packageList)


handleDeleteMigration : (Msg -> Msgs.Msg) -> String -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteMigration wrapMsg uuid appState model =
    ( { model | deletingMigration = Loading }
    , Cmd.map wrapMsg <| KnowledgeModelsApi.deleteMigration uuid appState DeleteKnowledgeModelCompleted
    )


deleteMigrationCompleted : (Msg -> Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
deleteMigrationCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            ( { model | deletingMigration = Success "Migration was successfully canceled", knowledgeModels = Loading }
            , fetchData wrapMsg appState
            )

        Err error ->
            ( { model | deletingMigration = getServerError error "Migration could not be deleted" }
            , getResultCmd result
            )
