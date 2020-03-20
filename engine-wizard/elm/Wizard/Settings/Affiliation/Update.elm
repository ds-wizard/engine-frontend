module Wizard.Settings.Affiliation.Update exposing
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
import Wizard.Settings.Affiliation.Models exposing (Model)
import Wizard.Settings.Affiliation.Msgs exposing (Msg(..))
import Wizard.Settings.Common.AffiliationConfigForm as AffiliationConfigForm
import Wizard.Settings.Common.EditableAffiliationConfig as EditableAffiliationConfig exposing (EditableAffiliationConfig)


fetchData : AppState -> Cmd Msg
fetchData appState =
    ConfigsApi.getAffiliationConfig appState GetAffiliationConfigCompleted


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GetAffiliationConfigCompleted result ->
            handleGetAffiliationConfigCompleted appState model result

        PutAffiliationConfigCompleted result ->
            handlePutAffiliationConfigCompleted appState model result

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model


handleGetAffiliationConfigCompleted : AppState -> Model -> Result ApiError EditableAffiliationConfig -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetAffiliationConfigCompleted appState model result =
    let
        newModel =
            case result of
                Ok config ->
                    { model | form = AffiliationConfigForm.init config, config = Success config }

                Err error ->
                    { model | config = ApiError.toActionResult (lg "apiError.config.affiliation.getError" appState) error }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


handlePutAffiliationConfigCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutAffiliationConfigCompleted appState model result =
    let
        newResult =
            case result of
                Ok _ ->
                    Success <| lg "apiSuccess.config.affiliation.put" appState

                Err error ->
                    ApiError.toActionResult (lg "apiError.config.affiliation.putError" appState) error

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
                    EditableAffiliationConfig.encode <| AffiliationConfigForm.toEditableAffiliationConfig form

                cmd =
                    Cmd.map wrapMsg <|
                        ConfigsApi.putAffiliationConfig body appState PutAffiliationConfigCompleted
            in
            ( { model | savingConfig = Loading }, cmd )

        _ ->
            let
                form =
                    Form.update AffiliationConfigForm.validation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )
