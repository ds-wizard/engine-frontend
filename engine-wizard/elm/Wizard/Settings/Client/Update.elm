module Wizard.Settings.Client.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Form
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.Api.Configs as ConfigApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Settings.Client.Models exposing (Model)
import Wizard.Settings.Client.Msgs exposing (Msg(..))
import Wizard.Settings.Common.ClientConfigForm as ClientConfigForm
import Wizard.Settings.Common.EditableClientConfig as EditableClientConfig exposing (EditableClientConfig)


fetchData : AppState -> Cmd Msg
fetchData appState =
    ConfigApi.getClientConfig appState GetClientConfigCompleted


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GetClientConfigCompleted result ->
            handleGetClientConfigCompleted appState model result

        PutClientConfigCompleted result ->
            handlePutClientConfigCompleted appState model result

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model


handleGetClientConfigCompleted : AppState -> Model -> Result ApiError EditableClientConfig -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetClientConfigCompleted appState model result =
    let
        newModel =
            case result of
                Ok config ->
                    { model | form = ClientConfigForm.init config, config = Success config }

                Err error ->
                    { model | config = ApiError.toActionResult (lg "apiError.config.client.getError" appState) error }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


handlePutClientConfigCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutClientConfigCompleted appState model result =
    let
        newResult =
            case result of
                Ok _ ->
                    Success <| lg "apiSuccess.config.client.put" appState

                Err error ->
                    ApiError.toActionResult (lg "apiError.config.client.putError" appState) error

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
                    EditableClientConfig.encode <| ClientConfigForm.toEditableClientConfig form

                cmd =
                    Cmd.map wrapMsg <|
                        ConfigApi.putClientConfig body appState PutClientConfigCompleted
            in
            ( { model | savingConfig = Loading }, cmd )

        _ ->
            let
                form =
                    Form.update ClientConfigForm.validation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )
