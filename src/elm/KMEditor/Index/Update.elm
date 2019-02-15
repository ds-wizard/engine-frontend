module KMEditor.Index.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Form
import Jwt
import KMEditor.Common.Models exposing (KnowledgeModel)
import KMEditor.Index.Models exposing (KnowledgeModelUpgradeForm, Model, encodeKnowledgeModelUpgradeForm, knowledgeModelUpgradeFormValidation)
import KMEditor.Index.Msgs exposing (Msg(..))
import KMEditor.Requests exposing (deleteKnowledgeModel, deleteMigration, getKnowledgeModels, postMigration)
import KMEditor.Routing exposing (Route(..))
import KMPackages.Common.Models exposing (PackageDetail)
import KMPackages.Requests exposing (getPackagesFiltered)
import List.Extra as List
import Models exposing (State)
import Msgs
import Requests exposing (getResultCmd)
import Routing exposing (Route(..), cmdNavigate)
import Utils exposing (versionIsGreater)


fetchData : (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchData wrapMsg session =
    getKnowledgeModels session
        |> Jwt.send GetKnowledgeModelsCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        GetKnowledgeModelsCompleted result ->
            getKnowledgeModelsCompleted model result

        ShowHideDeleteKnowledgeModal km ->
            ( { model | kmToBeDeleted = km, deletingKnowledgeModel = Unset }, Cmd.none )

        DeleteKnowledgeModel ->
            handleDeleteKM wrapMsg state.session model

        DeleteKnowledgeModelCompleted result ->
            deleteKnowledgeModelCompleted state model result

        PostMigrationCompleted result ->
            postMigrationCompleted state model result

        ShowHideUpgradeModal km ->
            handleShowHideUpgradeModal wrapMsg km model state.session

        UpgradeFormMsg formMsg ->
            handleUpgradeForm formMsg wrapMsg state.session model

        GetPackagesCompleted result ->
            handleGetPackagesCompleted model result

        DeleteMigration uuid ->
            handleDeleteMigration wrapMsg uuid state.session model

        DeleteMigrationCompleted result ->
            deleteMigrationCompleted wrapMsg state.session model result


getKnowledgeModelsCompleted : Model -> Result Jwt.JwtError (List KnowledgeModel) -> ( Model, Cmd Msgs.Msg )
getKnowledgeModelsCompleted model result =
    let
        newModel =
            case result of
                Ok knowledgeModels ->
                    { model | knowledgeModels = Success knowledgeModels }

                Err error ->
                    { model | knowledgeModels = getServerErrorJwt error "Unable to fetch knowledge models" }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


handleDeleteKM : (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteKM wrapMsg session model =
    case model.kmToBeDeleted of
        Just km ->
            ( { model | deletingKnowledgeModel = Loading }
            , deleteKnowledgeModelCmd wrapMsg km.uuid session
            )

        _ ->
            ( model, Cmd.none )


deleteKnowledgeModelCmd : (Msg -> Msgs.Msg) -> String -> Session -> Cmd Msgs.Msg
deleteKnowledgeModelCmd wrapMsg kmId session =
    deleteKnowledgeModel kmId session
        |> Jwt.send DeleteKnowledgeModelCompleted
        |> Cmd.map wrapMsg


deleteKnowledgeModelCompleted : State -> Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
deleteKnowledgeModelCompleted state model result =
    case result of
        Ok km ->
            ( model, cmdNavigate state.key <| KMEditor IndexRoute )

        Err error ->
            ( { model | deletingKnowledgeModel = getServerErrorJwt error "Knowledge model could not be deleted" }
            , getResultCmd result
            )


postMigrationCompleted : State -> Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
postMigrationCompleted state model result =
    case result of
        Ok migration ->
            let
                kmUuid =
                    model.kmToBeUpgraded
                        |> Maybe.andThen (\km -> Just km.uuid)
                        |> Maybe.withDefault ""
            in
            ( model, cmdNavigate state.key <| KMEditor <| MigrationRoute kmUuid )

        Err error ->
            ( { model | creatingMigration = getServerErrorJwt error "Migration could not be created" }
            , getResultCmd result
            )


handleShowHideUpgradeModal : (Msg -> Msgs.Msg) -> Maybe KnowledgeModel -> Model -> Session -> ( Model, Cmd Msgs.Msg )
handleShowHideUpgradeModal wrapMsg maybeKm model session =
    let
        getPackages lastAppliedParentPackageId =
            let
                cmd =
                    getPackagesFilteredCmd wrapMsg lastAppliedParentPackageId session
            in
            Just ( { model | kmToBeUpgraded = maybeKm, packages = Loading }, cmd )
    in
    maybeKm
        |> Maybe.andThen .lastAppliedParentPackageId
        |> Maybe.andThen getPackages
        |> Maybe.withDefault ( { model | kmToBeUpgraded = Nothing, packages = Unset }, Cmd.none )


getPackagesFilteredCmd : (Msg -> Msgs.Msg) -> String -> Session -> Cmd Msgs.Msg
getPackagesFilteredCmd wrapMsg lastAppliedParentPackageId session =
    let
        parts =
            String.split ":" lastAppliedParentPackageId
    in
    case ( List.head parts, List.getAt 1 parts ) of
        ( Just organizationId, Just kmId ) ->
            getPackagesFiltered organizationId kmId session
                |> Jwt.send GetPackagesCompleted
                |> Cmd.map wrapMsg

        _ ->
            Cmd.none


handleUpgradeForm : Form.Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleUpgradeForm formMsg wrapMsg session model =
    case ( formMsg, Form.getOutput model.kmUpgradeForm, model.kmToBeUpgraded ) of
        ( Form.Submit, Just kmUpgradeForm, Just km ) ->
            ( { model | creatingMigration = Loading }
            , postMigrationCmd wrapMsg session kmUpgradeForm km.uuid
            )

        _ ->
            ( { model | kmUpgradeForm = Form.update knowledgeModelUpgradeFormValidation formMsg model.kmUpgradeForm }
            , Cmd.none
            )


postMigrationCmd : (Msg -> Msgs.Msg) -> Session -> KnowledgeModelUpgradeForm -> String -> Cmd Msgs.Msg
postMigrationCmd wrapMsg session form uuid =
    form
        |> encodeKnowledgeModelUpgradeForm
        |> postMigration session uuid
        |> Jwt.send PostMigrationCompleted
        |> Cmd.map wrapMsg


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
            ( { model | packages = getServerErrorJwt error "Unable to get package list" }
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


handleDeleteMigration : (Msg -> Msgs.Msg) -> String -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteMigration wrapMsg uuid session model =
    ( { model | deletingMigration = Loading }, deleteMigrationCmd wrapMsg uuid session )


deleteMigrationCmd : (Msg -> Msgs.Msg) -> String -> Session -> Cmd Msgs.Msg
deleteMigrationCmd wrapMsg uuid session =
    deleteMigration uuid session
        |> Jwt.send DeleteKnowledgeModelCompleted
        |> Cmd.map wrapMsg


deleteMigrationCompleted : (Msg -> Msgs.Msg) -> Session -> Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
deleteMigrationCompleted wrapMsg session model result =
    case result of
        Ok km ->
            ( { model | deletingMigration = Success "Migration was successfully canceled", knowledgeModels = Loading }
            , fetchData wrapMsg session
            )

        Err error ->
            ( { model | deletingMigration = getServerErrorJwt error "Migration could not be deleted" }
            , getResultCmd result
            )
