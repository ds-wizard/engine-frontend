module KMEditor.Create.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Branches as BranchesApi
import Common.Api.Packages as PackagesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Form exposing (setFormErrors)
import Form exposing (Form)
import KMEditor.Common.Branch exposing (Branch)
import KMEditor.Common.BranchCreateForm as BranchCreateForm
import KMEditor.Create.Models exposing (..)
import KMEditor.Create.Msgs exposing (Msg(..))
import KMEditor.Routing exposing (Route(..))
import KnowledgeModels.Common.Package exposing (Package)
import Msgs
import Result exposing (Result)
import Routing exposing (Route(..), cmdNavigate)


fetchData : (Msg -> Msgs.Msg) -> AppState -> Cmd Msgs.Msg
fetchData wrapMsg appState =
    Cmd.map wrapMsg <|
        PackagesApi.getPackages appState GetPackagesCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetPackagesCompleted result ->
            handleGetPackageCompleted model result

        FormMsg formMsg ->
            handleFormMsg wrapMsg formMsg appState model

        PostBranchCompleted result ->
            handlePostBranchCompleted appState model result



-- Handlers


handleGetPackageCompleted : Model -> Result ApiError (List Package) -> ( Model, Cmd Msgs.Msg )
handleGetPackageCompleted model result =
    let
        newModel =
            case result of
                Ok packages ->
                    setSelectedPackage { model | packages = Success packages } packages

                Err error ->
                    { model | packages = getServerError error "Unable to get knowledge model list." }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


handleFormMsg : (Msg -> Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
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


handlePostBranchCompleted : AppState -> Model -> Result ApiError Branch -> ( Model, Cmd Msgs.Msg )
handlePostBranchCompleted appState model result =
    case result of
        Ok km ->
            ( model
            , cmdNavigate appState.key (Routing.KMEditor <| EditorRoute km.uuid)
            )

        Err error ->
            ( { model
                | form = setFormErrors error model.form
                , savingBranch = getServerError error "Knowledge model could not be created."
              }
            , getResultCmd result
            )
