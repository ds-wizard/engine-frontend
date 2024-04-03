module Wizard.Tenants.Create.View exposing (view)

import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div, form)
import Html.Events exposing (onSubmit)
import Shared.Form.FormError exposing (FormError)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Tenants.Common.TenantCreateForm exposing (TenantCreateForm)
import Wizard.Tenants.Create.Models exposing (Model)
import Wizard.Tenants.Create.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    form [ onSubmit (FormMsg Form.Submit), detailClass "Tenants__Create" ]
        [ Page.header (gettext "Create tenant" appState.locale) []
        , FormResult.view appState model.savingTenant
        , Html.map FormMsg <| formView appState model.form
        , FormActions.viewSubmit appState
            Cancel
            (ActionButton.SubmitConfig (gettext "Create" appState.locale) model.savingTenant)
        ]


formView : AppState -> Form FormError TenantCreateForm -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.input appState form "tenantId" (gettext "Tenant ID" appState.locale)
        , FormGroup.input appState form "tenantName" (gettext "Name" appState.locale)
        , FormGroup.input appState form "email" (gettext "Email" appState.locale)
        , FormGroup.input appState form "firstName" (gettext "First name" appState.locale)
        , FormGroup.input appState form "lastName" (gettext "Last name" appState.locale)
        ]
