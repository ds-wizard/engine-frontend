module Wizard.Pages.Tenants.Create.View exposing (view)

import Form exposing (Form)
import Html exposing (Html, div, form)
import Html.Events exposing (onSubmit)
import Shared.Components.ActionButton as ActionButton
import Shared.Components.FormGroup as FormGroup
import Shared.Components.FormResult as FormResult
import Shared.Components.Page as Page
import Shared.Utils.Form.FormError exposing (FormError)
import Wizard.Components.FormActions as FormActions
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Tenants.Common.TenantCreateForm exposing (TenantCreateForm)
import Wizard.Pages.Tenants.Create.Models exposing (Model)
import Wizard.Pages.Tenants.Create.Msgs exposing (Msg(..))
import Wizard.Utils.HtmlAttributesUtils exposing (detailClass)


view : AppState -> Model -> Html Msg
view appState model =
    form [ onSubmit (FormMsg Form.Submit), detailClass "Tenants__Create" ]
        [ Page.header "Create Tenant" []
        , FormResult.view model.savingTenant
        , Html.map FormMsg <| formView appState model.form
        , FormActions.viewSubmit appState
            Cancel
            (ActionButton.SubmitConfig "Create" model.savingTenant)
        ]


formView : AppState -> Form FormError TenantCreateForm -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.input appState.locale form "tenantId" "Tenant ID"
        , FormGroup.input appState.locale form "tenantName" "Name"
        , FormGroup.input appState.locale form "email" "Email"
        , FormGroup.input appState.locale form "firstName" "First name"
        , FormGroup.input appState.locale form "lastName" "Last name"
        ]
