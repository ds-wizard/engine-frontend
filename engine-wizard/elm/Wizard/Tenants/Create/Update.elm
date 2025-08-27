module Wizard.Tenants.Create.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Form
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Utils.Form as Form
import Shared.Utils.RequestHelpers as RequestHelpers
import Wizard.Api.Tenants as TenantsApi
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
            ( model, Ports.historyBack (Routing.toUrl Routes.tenantsIndex) )

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
                        TenantsApi.postTenant appState body PostAppComplete
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
                , form = Form.setFormErrors appState error model.form
              }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )
