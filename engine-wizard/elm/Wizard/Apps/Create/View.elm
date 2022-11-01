module Wizard.Apps.Create.View exposing (view)

import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div, form)
import Html.Events exposing (onSubmit)
import Shared.Form.FormError exposing (FormError)
import Wizard.Apps.Common.AppCreateForm exposing (AppCreateForm)
import Wizard.Apps.Create.Models exposing (Model)
import Wizard.Apps.Create.Msgs exposing (Msg(..))
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    form [ onSubmit (FormMsg Form.Submit), detailClass "Apps__Create" ]
        [ Page.header (gettext "Create app" appState.locale) []
        , FormResult.view appState model.savingApp
        , Html.map FormMsg <| formView appState model.form
        , FormActions.viewSubmit appState
            Routes.appsIndex
            (ActionButton.SubmitConfig (gettext "Create" appState.locale) model.savingApp)
        ]


formView : AppState -> Form FormError AppCreateForm -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.input appState form "appId" (gettext "App ID" appState.locale)
        , FormGroup.input appState form "appName" (gettext "Name" appState.locale)
        , FormGroup.input appState form "email" (gettext "Email" appState.locale)
        , FormGroup.input appState form "firstName" (gettext "First name" appState.locale)
        , FormGroup.input appState form "lastName" (gettext "Last name" appState.locale)
        ]
