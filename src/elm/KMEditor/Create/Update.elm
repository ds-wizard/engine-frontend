module KMEditor.Create.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.KnowledgeModels as KnowledgeModelsApi
import Common.Api.Packages as PackagesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Form exposing (setFormErrors)
import Form exposing (Form)
import KMEditor.Common.Models exposing (KnowledgeModel)
import KMEditor.Create.Models exposing (..)
import KMEditor.Create.Msgs exposing (Msg(..))
import KMEditor.Routing exposing (Route(..))
import KnowledgeModels.Common.Models exposing (PackageDetail)
import Msgs
import Routing exposing (Route(..), cmdNavigate)


fetchData : (Msg -> Msgs.Msg) -> AppState -> Cmd Msgs.Msg
fetchData wrapMsg appState =
    Cmd.map wrapMsg <|
        PackagesApi.getPackages appState GetPackagesCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetPackagesCompleted result ->
            getPackageCompleted model result

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model

        PostKnowledgeModelCompleted result ->
            postKmCompleted appState model result


getPackageCompleted : Model -> Result ApiError (List PackageDetail) -> ( Model, Cmd Msgs.Msg )
getPackageCompleted model result =
    let
        newModel =
            case result of
                Ok packages ->
                    setSelectedPackage { model | packages = Success packages } packages

                Err error ->
                    { model | packages = getServerError error "Unable to get package list" }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


setSelectedPackage : Model -> List PackageDetail -> Model
setSelectedPackage model packages =
    case model.selectedPackage of
        Just id ->
            if List.any (.id >> (==) id) packages then
                { model | form = initKnowledgeModelCreateForm model.selectedPackage }

            else
                model

        _ ->
            model


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just kmCreateForm ) ->
            let
                body =
                    encodeKnowledgeCreateModelForm kmCreateForm

                cmd =
                    Cmd.map wrapMsg <|
                        KnowledgeModelsApi.postKnowledgeModel body appState PostKnowledgeModelCompleted
            in
            ( { model | savingKnowledgeModel = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update knowledgeModelCreateFormValidation formMsg model.form }
            in
            ( newModel, Cmd.none )


postKmCompleted : AppState -> Model -> Result ApiError KnowledgeModel -> ( Model, Cmd Msgs.Msg )
postKmCompleted appState model result =
    case result of
        Ok km ->
            ( model
            , cmdNavigate appState.key (Routing.KMEditor <| EditorRoute km.uuid)
            )

        Err error ->
            ( { model
                | form = setFormErrors error model.form
                , savingKnowledgeModel = getServerError error "Knowledge model could not be created."
              }
            , getResultCmd result
            )
