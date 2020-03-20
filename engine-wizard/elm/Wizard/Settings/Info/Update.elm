module Wizard.Settings.Info.Update exposing
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
import Wizard.Settings.Common.EditableInfoConfig as EditableInfoConfig exposing (EditableInfoConfig)
import Wizard.Settings.Common.InfoConfigForm as InfoConfigForm
import Wizard.Settings.Info.Models exposing (Model)
import Wizard.Settings.Info.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    ConfigsApi.getInfoConfig appState GetInfoConfigCompleted


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GetInfoConfigCompleted result ->
            handleGetInfoConfigCompleted appState model result

        PutInfoConfigCompleted result ->
            handlePutInfoConfigCompleted appState model result

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model


handleGetInfoConfigCompleted : AppState -> Model -> Result ApiError EditableInfoConfig -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetInfoConfigCompleted appState model result =
    let
        newModel =
            case result of
                Ok config ->
                    { model | form = InfoConfigForm.init config, config = Success config }

                Err error ->
                    { model | config = ApiError.toActionResult (lg "apiError.config.info.getError" appState) error }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


handlePutInfoConfigCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutInfoConfigCompleted appState model result =
    let
        newResult =
            case result of
                Ok _ ->
                    Success <| lg "apiSuccess.config.info.put" appState

                Err error ->
                    ApiError.toActionResult (lg "apiError.config.info.putError" appState) error

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
                    EditableInfoConfig.encode <| InfoConfigForm.toEditableInfoConfig form

                cmd =
                    Cmd.map wrapMsg <|
                        ConfigsApi.putInfoConfig body appState PutInfoConfigCompleted
            in
            ( { model | savingConfig = Loading }, cmd )

        _ ->
            let
                form =
                    Form.update InfoConfigForm.validation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )
