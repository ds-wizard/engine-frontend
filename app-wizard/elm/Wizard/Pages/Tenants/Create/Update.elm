module Wizard.Pages.Tenants.Create.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Ports.Dom as Dom
import Common.Ports.Window as Window
import Common.Utils.Form as Form
import Common.Utils.RequestHelpers as RequestHelpers
import Form
import Wizard.Api.Tenants as TenantsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Tenants.Common.TenantCreateForm as AppCreateForm
import Wizard.Pages.Tenants.Create.Models exposing (Model)
import Wizard.Pages.Tenants.Create.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)


fetchData : Cmd Msg
fetchData =
    Dom.focus "#tenantId"


update : AppState -> Msg -> (Msg -> Wizard.Msgs.Msg) -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update appState msg wrapMsg model =
    case msg of
        Cancel ->
            ( model, Window.historyBack (Routing.toUrl Routes.tenantsIndex) )

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
