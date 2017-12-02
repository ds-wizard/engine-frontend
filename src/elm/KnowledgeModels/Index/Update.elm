module KnowledgeModels.Index.Update exposing (..)

import Auth.Models exposing (Session)
import Common.Types exposing (ActionResult(..))
import Form
import Jwt
import KnowledgeModels.Index.Models exposing (Model)
import KnowledgeModels.Index.Msgs exposing (Msg(..))
import KnowledgeModels.Models exposing (KnowledgeModel, KnowledgeModelUpgradeForm, encodeKnowledgeModelUpgradeForm, knowledgeModelUpgradeFormValidation)
import KnowledgeModels.Requests exposing (deleteKnowledgeModel, deleteMigration, getKnowledgeModels, postMigration)
import List.Extra as List
import Msgs
import PackageManagement.Models exposing (PackageDetail)
import PackageManagement.Requests exposing (getPackagesFiltered)
import Requests exposing (toCmd)
import Routing exposing (Route(..), cmdNavigate)
import Utils exposing (versionIsGreater)


getKnowledgeModelsCmd : Session -> Cmd Msgs.Msg
getKnowledgeModelsCmd session =
    getKnowledgeModels session
        |> toCmd GetKnowledgeModelsCompleted Msgs.KnowledgeModelsIndexMsg


deleteKnowledgeModelCmd : String -> Session -> Cmd Msgs.Msg
deleteKnowledgeModelCmd kmId session =
    deleteKnowledgeModel kmId session
        |> toCmd DeleteKnowledgeModelCompleted Msgs.KnowledgeModelsIndexMsg


postMigrationCmd : Session -> KnowledgeModelUpgradeForm -> String -> Cmd Msgs.Msg
postMigrationCmd session form uuid =
    form
        |> encodeKnowledgeModelUpgradeForm
        |> postMigration session uuid
        |> toCmd PostMigrationCompleted Msgs.KnowledgeModelsIndexMsg


getPackagesFilteredCmd : String -> Session -> Cmd Msgs.Msg
getPackagesFilteredCmd lastAppliedParentPackageId session =
    let
        parts =
            String.split ":" lastAppliedParentPackageId
    in
    case ( List.head parts, List.getAt 1 parts ) of
        ( Just groupId, Just artifactId ) ->
            getPackagesFiltered groupId artifactId session
                |> toCmd GetPackagesCompleted Msgs.KnowledgeModelsIndexMsg

        _ ->
            Cmd.none


deleteMigrationCmd : String -> Session -> Cmd Msgs.Msg
deleteMigrationCmd uuid session =
    deleteMigration uuid session
        |> toCmd DeleteMigrationCompleted Msgs.KnowledgeModelsIndexMsg


getKnowledgeModelsCompleted : Model -> Result Jwt.JwtError (List KnowledgeModel) -> ( Model, Cmd Msgs.Msg )
getKnowledgeModelsCompleted model result =
    let
        newModel =
            case result of
                Ok knowledgeModels ->
                    { model | knowledgeModels = Success knowledgeModels }

                Err error ->
                    { model | knowledgeModels = Error "Unable to fetch knowledge models" }
    in
    ( newModel, Cmd.none )


handleDeleteKM : Session -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteKM session model =
    case model.kmToBeDeleted of
        Just km ->
            ( { model | deletingKnowledgeModel = Loading }
            , deleteKnowledgeModelCmd km.uuid session
            )

        _ ->
            ( model, Cmd.none )


deleteKnowledgeModelCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
deleteKnowledgeModelCompleted model result =
    case result of
        Ok km ->
            ( model, cmdNavigate KnowledgeModels )

        Err error ->
            ( { model | deletingKnowledgeModel = Error "Knowledge model could not be deleted" }
            , Cmd.none
            )


handleShowHideUpgradeModal : Maybe KnowledgeModel -> Model -> Session -> ( Model, Cmd Msgs.Msg )
handleShowHideUpgradeModal maybeKm model session =
    let
        getPackages lastAppliedParentPackageId =
            let
                cmd =
                    getPackagesFilteredCmd lastAppliedParentPackageId session
            in
            Just ( { model | kmToBeUpgraded = maybeKm, packages = Loading }, cmd )
    in
    maybeKm
        |> Maybe.andThen .lastAppliedParentPackageId
        |> Maybe.andThen getPackages
        |> Maybe.withDefault ( { model | kmToBeUpgraded = Nothing, packages = Unset }, Cmd.none )


filterPackages : List PackageDetail -> KnowledgeModel -> Maybe (List PackageDetail)
filterPackages packageList knowledgeModel =
    let
        getFilteredList packages version =
            Just <| List.filter (.version >> versionIsGreater version) packages
    in
    knowledgeModel.lastAppliedParentPackageId
        |> Maybe.andThen (String.split ":" >> List.getAt 2)
        |> Maybe.andThen (getFilteredList packageList)


handleGetPackagesCompleted : Model -> Result Jwt.JwtError (List PackageDetail) -> ( Model, Cmd Msgs.Msg )
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
            ( { model | packages = Error "Unable to get package list" }, Cmd.none )


handleUpgradeForm : Form.Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleUpgradeForm formMsg session model =
    case ( formMsg, Form.getOutput model.kmUpgradeForm, model.kmToBeUpgraded ) of
        ( Form.Submit, Just kmUpgradeForm, Just km ) ->
            let
                cmd =
                    postMigrationCmd session kmUpgradeForm km.uuid
            in
            ( { model | creatingMigration = Loading }, cmd )

        _ ->
            ( { model | kmUpgradeForm = Form.update knowledgeModelUpgradeFormValidation formMsg model.kmUpgradeForm }
            , Cmd.none
            )


postMigrationCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
postMigrationCompleted model result =
    case result of
        Ok migration ->
            let
                kmUuid =
                    model.kmToBeUpgraded
                        |> Maybe.andThen (\km -> Just km.uuid)
                        |> Maybe.withDefault ""
            in
            ( model, cmdNavigate <| KnowledgeModelsMigration kmUuid )

        Err error ->
            ( { model | creatingMigration = Error "Migration could not be created" }, Cmd.none )


handleDeleteMigration : String -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteMigration uuid session model =
    ( { model | deletingMigration = Loading }, deleteMigrationCmd uuid session )


deleteMigrationCompleted : Session -> Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
deleteMigrationCompleted session model result =
    case result of
        Ok km ->
            ( { model | deletingMigration = Success "Migration was successfully canceled", knowledgeModels = Loading }
            , getKnowledgeModelsCmd session
            )

        Err error ->
            ( { model | deletingMigration = Error "Migration could not be deleted" }
            , Cmd.none
            )


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetKnowledgeModelsCompleted result ->
            getKnowledgeModelsCompleted model result

        ShowHideDeleteKnowledgeModal km ->
            ( { model | kmToBeDeleted = km, deletingKnowledgeModel = Unset }, Cmd.none )

        DeleteKnowledgeModel ->
            handleDeleteKM session model

        DeleteKnowledgeModelCompleted result ->
            deleteKnowledgeModelCompleted model result

        PostMigrationCompleted result ->
            postMigrationCompleted model result

        ShowHideUpgradeModal km ->
            handleShowHideUpgradeModal km model session

        UpgradeFormMsg msg ->
            handleUpgradeForm msg session model

        GetPackagesCompleted result ->
            handleGetPackagesCompleted model result

        DeleteMigration uuid ->
            handleDeleteMigration uuid session model

        DeleteMigrationCompleted result ->
            deleteMigrationCompleted session model result
