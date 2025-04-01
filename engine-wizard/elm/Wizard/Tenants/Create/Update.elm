module Wizard.Tenants.Create.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Form
import Shared.Api.Tenants as TenantsApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form exposing (setFormErrors)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)
import Wizard.Tenants.Common.TenantCreateForm as AppCreateForm
import Wizard.Tenants.Create.Models exposing (Model)
import Wizard.Tenants.Create.Msgs exposing (Msg(..))


update : AppState -> Msg -> (Msg -> Wizard.Msgs.Msg) -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update appState msg wrapMsg model =
    case msg of
        Cancel ->
            ( model, Ports.historyBack (Routing.toUrl appState Routes.tenantsIndex) )

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model

        PostAppComplete result ->
            postAppCompleted appState model result


handleForm : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just appCreateForm ) ->
            let
                body =
                    AppCreateForm.encode appCreateForm

                cmd =
                    Cmd.map wrapMsg <|
                        TenantsApi.postTenant body appState PostAppComplete
            in
            ( { model | savingTenant = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update AppCreateForm.validation formMsg model.form }
            in
            ( newModel, Cmd.none )


postAppCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
postAppCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState Routes.tenantsIndex )

        Err error ->
            ( { model
                | savingTenant = ApiError.toActionResult appState "Tenant could not be created." error
                , form = setFormErrors appState error model.form
              }
            , getResultCmd Wizard.Msgs.logoutMsg result
            )
