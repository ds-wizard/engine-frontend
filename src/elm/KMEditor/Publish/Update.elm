module KMEditor.Publish.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Branches as BranchesApi
import Common.Api.Packages as PackagesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Form
import Form.Field as Field
import KMEditor.Common.BranchDetail exposing (BranchDetail)
import KMEditor.Common.BranchPublishForm as BranchPublishForm
import KMEditor.Publish.Models exposing (Model)
import KMEditor.Publish.Msgs exposing (Msg(..))
import KnowledgeModels.Common.PackageDetail exposing (PackageDetail)
import KnowledgeModels.Common.Version as Version exposing (Version)
import KnowledgeModels.Routing
import Msgs
import Routing exposing (Route(..), cmdNavigate)


fetchData : String -> AppState -> Cmd Msg
fetchData uuid appState =
    BranchesApi.getBranch uuid appState GetBranchCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
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


handleGetBranchCompleted : (Msg -> Msgs.Msg) -> AppState -> Model -> Result ApiError BranchDetail -> ( Model, Cmd Msgs.Msg )
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
            ( { model | branch = getServerError error "Unable to get the knowledge model." }
            , getResultCmd result
            )


handleGetPreviousPackageCompleted : Model -> Result ApiError PackageDetail -> ( Model, Cmd Msgs.Msg )
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


handleFormMsg : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
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


handleFormSetVersion : Version -> Model -> ( Model, Cmd Msgs.Msg )
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


handlePutBranchCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
handlePutBranchCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState.key (KnowledgeModels KnowledgeModels.Routing.Index) )

        Err error ->
            ( { model | publishingBranch = getServerError error "Publishing new version failed" }
            , getResultCmd result
            )
