module Wizard.Pages.Tenants.Create.View exposing (view)

import Common.Components.Container as Container
import Common.Components.Form as Form
import Common.Components.FormGroup as FormGroup
import Common.Components.Page as Page
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Html exposing (Html, div)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Tenants.Common.TenantCreateForm exposing (TenantCreateForm)
import Wizard.Pages.Tenants.Create.Models exposing (Model)
import Wizard.Pages.Tenants.Create.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Container.simpleForm
        [ Page.header "Create Tenant" []
        , Form.simple
            { formMsg = FormMsg
            , formResult = model.savingTenant
            , formView = Html.map FormMsg <| formView appState model.form
            , submitLabel = "Create"
            , cancelMsg = Just Cancel
            , locale = appState.locale
            , isMac = appState.navigator.isMac
            }
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
