module Wizard.Pages.Tenants.Create.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Ports.Dom as Dom
import Common.Ports.FormUtils as FormUtils
import Common.Ports.Window as Window
import Common.Utils.Form as Form
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Form.Field as Field
import Maybe.Extra as Maybe
import String.Normalize as Normalize
import Wizard.Api.Tenants as TenantsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Tenants.Common.TenantCreateForm as AppCreateForm exposing (TenantCreateForm)
import Wizard.Pages.Tenants.Create.Models exposing (Model)
import Wizard.Pages.Tenants.Create.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)


fetchData : Cmd Msg
fetchData =
    Dom.focus "#tenantName"


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
                newForm =
                    Form.update AppCreateForm.validation formMsg model.form

                tenantIdEmpty =
                    Maybe.unwrap True String.isEmpty (Form.getFieldAsString "tenantId" model.form).value

                formWithTenantId =
                    case ( formMsg, tenantIdEmpty ) of
                        ( Form.Blur "tenantName", True ) ->
                            let
                                suggestedTenantId =
                                    (Form.getFieldAsString "tenantName" model.form).value
                                        |> Maybe.unwrap "" Normalize.slug
                            in
                            setTenantCreateFormValue "tenantId" suggestedTenantId newForm

                        _ ->
                            newForm
            in
            ( { model | form = formWithTenantId }, FormUtils.scrollToInvalidField formMsg )


setTenantCreateFormValue : String -> String -> Form FormError TenantCreateForm -> Form FormError TenantCreateForm
setTenantCreateFormValue field value =
    Form.update AppCreateForm.validation (Form.Input field Form.Text (Field.String value))


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
