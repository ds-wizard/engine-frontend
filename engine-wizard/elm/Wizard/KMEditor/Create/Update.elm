module Wizard.KMEditor.Create.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Result exposing (Result)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.Api.Branches as BranchesApi
import Wizard.Common.Api.Packages as PackagesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (setFormErrors)
import Wizard.KMEditor.Common.Branch exposing (Branch)
import Wizard.KMEditor.Common.BranchCreateForm as BranchCreateForm
import Wizard.KMEditor.Create.Models exposing (..)
import Wizard.KMEditor.Create.Msgs exposing (Msg(..))
import Wizard.KMEditor.Routes exposing (Route(..))
import Wizard.KnowledgeModels.Common.Package exposing (Package)
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Cmd Msg
fetchData appState =
    PackagesApi.getPackages appState GetPackagesCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetPackagesCompleted result ->
            handleGetPackageCompleted appState model result

        FormMsg formMsg ->
            handleFormMsg wrapMsg formMsg appState model

        PostBranchCompleted result ->
            handlePostBranchCompleted appState model result



-- Handlers


handleGetPackageCompleted : AppState -> Model -> Result ApiError (List Package) -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetPackageCompleted appState model result =
    let
        newModel =
            case result of
                Ok packages ->
                    setSelectedPackage { model | packages = Success packages } packages

                Err error ->
                    { model | packages = ApiError.toActionResult (lg "apiError.packages.getListError" appState) error }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


handleFormMsg : (Msg -> Wizard.Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleFormMsg wrapMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just kmCreateForm ) ->
            let
                body =
                    BranchCreateForm.encode kmCreateForm

                cmd =
                    Cmd.map wrapMsg <|
                        BranchesApi.postBranch body appState PostBranchCompleted
            in
            ( { model | savingBranch = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update BranchCreateForm.validation formMsg model.form }
            in
            ( newModel, Cmd.none )


handlePostBranchCompleted : AppState -> Model -> Result ApiError Branch -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostBranchCompleted appState model result =
    case result of
        Ok km ->
            ( model
            , cmdNavigate appState (Routes.KMEditorRoute <| EditorRoute km.uuid)
            )

        Err error ->
            ( { model
                | form = setFormErrors error model.form
                , savingBranch = ApiError.toActionResult (lg "apiError.branches.postError" appState) error
              }
            , getResultCmd result
            )
