module Wizard.Pages.KMEditor.Publish.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Ports.Window as Ports
import Common.Utils.RequestHelpers as RequestHelpers
import Form
import Form.Field as Field
import Gettext exposing (gettext)
import Uuid exposing (Uuid)
import Version exposing (Version)
import Wizard.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Wizard.Api.Models.KnowledgeModelEditorDetail exposing (KnowledgeModelEditorDetail)
import Wizard.Api.Models.Package exposing (Package)
import Wizard.Api.Models.PackageDetail exposing (PackageDetail)
import Wizard.Api.Packages as PackagesApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KMEditor.Common.KnowledgeModelEditorPublishForm as KnowledgeModelEditorPublishForm
import Wizard.Pages.KMEditor.Publish.Models exposing (Model)
import Wizard.Pages.KMEditor.Publish.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)


fetchData : Uuid -> AppState -> Cmd Msg
fetchData uuid appState =
    KnowledgeModelEditorsApi.getKnowledgeModelEditor appState uuid GetKnowledgeModelEditorCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetKnowledgeModelEditorCompleted result ->
            handleGetKnowledgeModelEditorCompleted wrapMsg appState model result

        GetPreviousPackageCompleted result ->
            handleGetPreviousPackageCompleted model result

        Cancel ->
            ( model, Ports.historyBack (Routing.toUrl Routes.knowledgeModelsIndex) )

        FormMsg formMsg ->
            handleFormMsg formMsg wrapMsg appState model

        FormSetVersion version ->
            handleFormSetVersion version model

        PutKnowledgeModelEditorCompleted result ->
            handlePutKnowledgeModelEditorCompleted appState model result



-- Handlers


handleGetKnowledgeModelEditorCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError KnowledgeModelEditorDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetKnowledgeModelEditorCompleted wrapMsg appState model result =
    case result of
        Ok kmEditor ->
            let
                cmd =
                    case kmEditor.previousPackageId of
                        Just previousPackageId ->
                            Cmd.map wrapMsg <|
                                PackagesApi.getPackage appState previousPackageId GetPreviousPackageCompleted

                        Nothing ->
                            Cmd.none
            in
            ( { model | kmEditor = Success kmEditor }
            , cmd
            )

        Err error ->
            ( { model | kmEditor = ApiError.toActionResult appState (gettext "Unable to get the knowledge model details." appState.locale) error }
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
                        |> Form.update KnowledgeModelEditorPublishForm.validation (formMsg "description" package.description)
                        |> Form.update KnowledgeModelEditorPublishForm.validation (formMsg "readme" package.readme)
                        |> Form.update KnowledgeModelEditorPublishForm.validation (formMsg "license" package.license)
            in
            ( { model | form = form }
            , Cmd.none
            )

        Err _ ->
            ( model, RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result )


handleFormMsg : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleFormMsg formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form, model.kmEditor ) of
        ( Form.Submit, Just form, Success kmEditor ) ->
            let
                body =
                    KnowledgeModelEditorPublishForm.encode kmEditor.uuid form

                cmd =
                    Cmd.map wrapMsg <|
                        PackagesApi.postFromMigration appState body PutKnowledgeModelEditorCompleted
            in
            ( { model | publishingKnowledgeModelEditor = Loading }, cmd )

        _ ->
            let
                form =
                    Form.update KnowledgeModelEditorPublishForm.validation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )


handleFormSetVersion : Version -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleFormSetVersion version model =
    let
        formMsg field value =
            Form.Input field Form.Text <| Field.String (String.fromInt value)

        form =
            model.form
                |> Form.update KnowledgeModelEditorPublishForm.validation (formMsg "major" <| Version.getMajor version)
                |> Form.update KnowledgeModelEditorPublishForm.validation (formMsg "minor" <| Version.getMinor version)
                |> Form.update KnowledgeModelEditorPublishForm.validation (formMsg "patch" <| Version.getPatch version)
    in
    ( { model | form = form }, Cmd.none )


handlePutKnowledgeModelEditorCompleted : AppState -> Model -> Result ApiError Package -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutKnowledgeModelEditorCompleted appState model result =
    case result of
        Ok package ->
            ( model, cmdNavigate appState (Routes.knowledgeModelsDetail package.id) )

        Err error ->
            ( { model | publishingKnowledgeModelEditor = ApiError.toActionResult appState (gettext "Publishing the new version failed." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )
