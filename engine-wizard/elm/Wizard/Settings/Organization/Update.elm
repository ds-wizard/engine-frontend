module Wizard.Settings.Organization.Update exposing
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
import Wizard.Settings.Common.EditableOrganizationConfig as EditableOrganizationConfig exposing (EditableOrganizationConfig)
import Wizard.Settings.Common.OrganizationConfigForm as OrganizationConfigForm
import Wizard.Settings.Organization.Models exposing (Model)
import Wizard.Settings.Organization.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    ConfigsApi.getOrganizationConfig appState GetOrganizationConfigCompleted


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GetOrganizationConfigCompleted result ->
            handleGetOrganizationConfigCompleted appState model result

        PutOrganizationConfigCompleted result ->
            handlePutOrganizationConfigCompleted appState model result

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model


handleGetOrganizationConfigCompleted : AppState -> Model -> Result ApiError EditableOrganizationConfig -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetOrganizationConfigCompleted appState model result =
    let
        newModel =
            case result of
                Ok config ->
                    { model | form = OrganizationConfigForm.init config, config = Success config }

                Err error ->
                    { model | config = ApiError.toActionResult (lg "apiError.config.organization.getError" appState) error }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


handlePutOrganizationConfigCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutOrganizationConfigCompleted appState model result =
    let
        newResult =
            case result of
                Ok _ ->
                    Success <| lg "apiSuccess.config.organization.put" appState

                Err error ->
                    ApiError.toActionResult (lg "apiError.config.organization.putError" appState) error

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
                    EditableOrganizationConfig.encode <| OrganizationConfigForm.toEditableOrganizationConfig form

                cmd =
                    Cmd.map wrapMsg <|
                        ConfigsApi.putOrganizationConfig body appState PutOrganizationConfigCompleted
            in
            ( { model | savingConfig = Loading }, cmd )

        _ ->
            let
                form =
                    Form.update OrganizationConfigForm.validation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )
