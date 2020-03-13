module Wizard.Settings.Update exposing (..)

import ActionResult exposing (ActionResult(..))
import Form
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.Api.Config as ConfigApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Settings.Common.ConfigForm as ConfigForm
import Wizard.Settings.Common.EditableConfig as EditableConfig exposing (EditableConfig)
import Wizard.Settings.Models exposing (Model)
import Wizard.Settings.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    ConfigApi.getApplicationConfig appState GetApplicationConfigCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetApplicationConfigCompleted result ->
            handleGetApplicationConfigCompleted appState model result

        PutApplicationConfigCompleted result ->
            handlePutApplicationConfigCompleted appState model result

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model


handleGetApplicationConfigCompleted : AppState -> Model -> Result ApiError EditableConfig -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetApplicationConfigCompleted appState model result =
    let
        newModel =
            case result of
                Ok config ->
                    { model | form = ConfigForm.init config, config = Success config }

                Err error ->
                    { model | config = ApiError.toActionResult (lg "apiError.config.application.getError" appState) error }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


handlePutApplicationConfigCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutApplicationConfigCompleted appState model result =
    let
        newResult =
            case result of
                Ok _ ->
                    Success <| lg "apiSuccess.config.application.put" appState

                Err error ->
                    ApiError.toActionResult (lg "apiError.config.application.putError" appState) error

        cmd =
            getResultCmd result
    in
    ( { model | savingConfig = newResult }, Cmd.batch [ cmd, Ports.scrollIntoView ".Configuration" ] )


handleForm : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    EditableConfig.encode <| ConfigForm.toEditableConfig form

                cmd =
                    Cmd.map wrapMsg <|
                        ConfigApi.putApplicationConfig body appState PutApplicationConfigCompleted
            in
            ( { model | savingConfig = Loading }, cmd )

        _ ->
            let
                form =
                    Form.update ConfigForm.validation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )
