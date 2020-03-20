module Wizard.Settings.Features.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Form
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.Api.Configs as ConfigsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Settings.Common.EditableFeaturesConfig as EditableFeaturesConfig exposing (EditableFeaturesConfig)
import Wizard.Settings.Common.FeaturesConfigForm as FeaturesConfigForm
import Wizard.Settings.Features.Models exposing (Model)
import Wizard.Settings.Features.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    ConfigsApi.getFeaturesConfig appState GetFeaturesConfigCompleted


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GetFeaturesConfigCompleted result ->
            handleGetFeaturesConfigCompleted appState model result

        PutFeaturesConfigCompleted result ->
            handlePutFeaturesConfigCompleted appState model result

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model


handleGetFeaturesConfigCompleted : AppState -> Model -> Result ApiError EditableFeaturesConfig -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetFeaturesConfigCompleted appState model result =
    let
        newModel =
            case result of
                Ok config ->
                    { model | form = FeaturesConfigForm.init config, config = Success config }

                Err error ->
                    { model | config = ApiError.toActionResult (lg "apiError.config.features.getError" appState) error }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


handlePutFeaturesConfigCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutFeaturesConfigCompleted appState model result =
    let
        newResult =
            case result of
                Ok _ ->
                    Success <| lg "apiSuccess.config.features.put" appState

                Err error ->
                    ApiError.toActionResult (lg "apiError.config.features.putError" appState) error

        cmd =
            getResultCmd result
    in
    ( { model | savingConfig = newResult }, Cmd.batch [ cmd, Ports.scrollToTop ".Settings__content" ] )


handleForm : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    EditableFeaturesConfig.encode <| FeaturesConfigForm.toEditableFeaturesConfig form

                cmd =
                    Cmd.map wrapMsg <|
                        ConfigsApi.putFeaturesConfig body appState PutFeaturesConfigCompleted
            in
            ( { model | savingConfig = Loading }, cmd )

        _ ->
            let
                form =
                    Form.update FeaturesConfigForm.validation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )
