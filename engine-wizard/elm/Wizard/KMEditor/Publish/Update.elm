module Wizard.KMEditor.Publish.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Form
import Form.Field as Field
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Version exposing (Version)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.Api.Branches as BranchesApi
import Wizard.Common.Api.Packages as PackagesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Common.BranchDetail exposing (BranchDetail)
import Wizard.KMEditor.Common.BranchPublishForm as BranchPublishForm
import Wizard.KMEditor.Publish.Models exposing (Model)
import Wizard.KMEditor.Publish.Msgs exposing (Msg(..))
import Wizard.KnowledgeModels.Common.PackageDetail exposing (PackageDetail)
import Wizard.KnowledgeModels.Routes
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : String -> AppState -> Cmd Msg
fetchData uuid appState =
    BranchesApi.getBranch uuid appState GetBranchCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetBranchCompleted result ->
            handleGetBranchCompleted wrapMsg appState model result

        GetPreviousPackageCompleted result ->
            handleGetPreviousPackageCompleted model result

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
                                PackagesApi.getPackage previousPackageId appState GetPreviousPackageCompleted

                        Nothing ->
                            Cmd.none
            in
            ( { model | branch = Success branch }
            , cmd
            )

        Err error ->
            ( { model | branch = ApiError.toActionResult (lg "apiError.branches.getError" appState) error }
            , getResultCmd result
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
            ( model, getResultCmd result )


handleFormMsg : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleFormMsg formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form, model.branch ) of
        ( Form.Submit, Just form, Success branch ) ->
            let
                ( version, body ) =
                    BranchPublishForm.encode form

                cmd =
                    Cmd.map wrapMsg <|
                        BranchesApi.putVersion branch.uuid version body appState PutBranchCompleted
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


handlePutBranchCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutBranchCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState (Routes.KnowledgeModelsRoute Wizard.KnowledgeModels.Routes.IndexRoute) )

        Err error ->
            ( { model | publishingBranch = ApiError.toActionResult (lg "apiError.packages.version.postError" appState) error }
            , getResultCmd result
            )
