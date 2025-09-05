module Wizard.Pages.KMEditor.Publish.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Data.ApiError as ApiError exposing (ApiError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form
import Form.Field as Field
import Gettext exposing (gettext)
import Uuid exposing (Uuid)
import Version exposing (Version)
import Wizard.Api.Branches as BranchesApi
import Wizard.Api.Models.BranchDetail exposing (BranchDetail)
import Wizard.Api.Models.Package exposing (Package)
import Wizard.Api.Models.PackageDetail exposing (PackageDetail)
import Wizard.Api.Packages as PackagesApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KMEditor.Common.BranchPublishForm as BranchPublishForm
import Wizard.Pages.KMEditor.Publish.Models exposing (Model)
import Wizard.Pages.KMEditor.Publish.Msgs exposing (Msg(..))
import Wizard.Ports as Ports
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)


fetchData : Uuid -> AppState -> Cmd Msg
fetchData uuid appState =
    BranchesApi.getBranch appState uuid GetBranchCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetBranchCompleted result ->
            handleGetBranchCompleted wrapMsg appState model result

        GetPreviousPackageCompleted result ->
            handleGetPreviousPackageCompleted model result

        Cancel ->
            ( model, Ports.historyBack (Routing.toUrl Routes.knowledgeModelsIndex) )

        FormMsg formMsg ->
            handleFormMsg formMsg wrapMsg appState model

        FormSetVersion version ->
            handleFormSetVersion version model

        PutBranchCompleted result ->
            handlePutBranchCompleted appState model result



-- Handlers


handleGetBranchCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError BranchDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetBranchCompleted wrapMsg appState model result =
    case result of
        Ok branch ->
            let
                cmd =
                    case branch.previousPackageId of
                        Just previousPackageId ->
                            Cmd.map wrapMsg <|
                                PackagesApi.getPackage appState previousPackageId GetPreviousPackageCompleted

                        Nothing ->
                            Cmd.none
            in
            ( { model | branch = Success branch }
            , cmd
            )

        Err error ->
            ( { model | branch = ApiError.toActionResult appState (gettext "Unable to get the knowledge model details." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )


handleGetPreviousPackageCompleted : Model -> Result ApiError PackageDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetPreviousPackageCompleted model result =
    case result of
        Ok package ->
            let
                formMsg field value =
                    Form.Input field Form.Text <| Field.String value

                form =
                    model.form
                        |> Form.update BranchPublishForm.validation (formMsg "description" package.description)
                        |> Form.update BranchPublishForm.validation (formMsg "readme" package.readme)
                        |> Form.update BranchPublishForm.validation (formMsg "license" package.license)
            in
            ( { model | form = form }
            , Cmd.none
            )

        Err _ ->
            ( model, RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result )


handleFormMsg : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleFormMsg formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form, model.branch ) of
        ( Form.Submit, Just form, Success branch ) ->
            let
                body =
                    BranchPublishForm.encode branch.uuid form

                cmd =
                    Cmd.map wrapMsg <|
                        PackagesApi.postFromMigration appState body PutBranchCompleted
            in
            ( { model | publishingBranch = Loading }, cmd )

        _ ->
            let
                form =
                    Form.update BranchPublishForm.validation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )


handleFormSetVersion : Version -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleFormSetVersion version model =
    let
        formMsg field value =
            Form.Input field Form.Text <| Field.String (String.fromInt value)

        form =
            model.form
                |> Form.update BranchPublishForm.validation (formMsg "major" <| Version.getMajor version)
                |> Form.update BranchPublishForm.validation (formMsg "minor" <| Version.getMinor version)
                |> Form.update BranchPublishForm.validation (formMsg "patch" <| Version.getPatch version)
    in
    ( { model | form = form }, Cmd.none )


handlePutBranchCompleted : AppState -> Model -> Result ApiError Package -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutBranchCompleted appState model result =
    case result of
        Ok package ->
            ( model, cmdNavigate appState (Routes.knowledgeModelsDetail package.id) )

        Err error ->
            ( { model | publishingBranch = ApiError.toActionResult appState (gettext "Publishing the new version failed." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )
